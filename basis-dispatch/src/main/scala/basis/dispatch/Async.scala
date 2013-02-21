/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis.dispatch

import basis.collections._
import basis.containers._
import basis.sequential.strict._

import java.util.concurrent.locks.LockSupport

import scala.annotation.{elidable, tailrec}
import scala.reflect.ClassTag
import scala.runtime.AbstractFunction1

/** A lock-free, work-stealing thread pool for asynchronous computations.
  * 
  * @author   Chris Sachs
  * @version  0.1
  * @since    0.1
  * 
  * @groupprio  Evaluating    1
  * @groupprio  Monitoring    2
  * @groupprio  Classifying   3
  * @groupprio  Workers       4
  */
abstract class Async extends Trace { async =>
  import MetaAsync._
  
  /** The (immutable) array of live workers. */
  @volatile private[dispatch] final var workers: Array[Worker] = new Array[Worker](0)
  
  /** The set of active workers. */
  @volatile private[dispatch] final var working: HashSet[Worker] = HashSet.empty[Worker]
  
  /** The set of blocked workers. */
  @volatile private[dispatch] final var blocked: HashSet[Worker] = HashSet.empty[Worker]
  
  /** The stack of inactive workers. */
  @volatile private[dispatch] final var waiting: List[Worker] = List.empty[Worker]
  
  /** The work submission queue. */
  @volatile private[dispatch] final var queue: Batch[() => _] = Batch.empty[() => _]
  
  /** The next unique worker identifier. */
  @volatile private[dispatch] final var nextUID: Int = 1
  
  /** Returns the target number of active threads. Configure the target
    * parallelism by setting the `basis.dispatch.async.parallelism` system
    * property to a natural number or a multiple of the available processors
    * (`x2`, `x4`, etc.)–defaults to the number of available processors (`x1`).
    * @group Classifying */
  val parallelism: Int = {
    def parse(s: String): Int = {
      var i = 0
      val n = if (s != null) s.length else 0
      val k = if (0 < n && s.charAt(0) == 'x') { i = 1; Runtime.getRuntime.availableProcessors } else 1
      var p = 0
      while (i < n) {
        val c = s.charAt(i)
        if (c >= '0' && c <= '9') { p = 10 * p + (c - '0'); i += 1 }
        else { p = 0; i = n }
      }
      if (p > 0) k * p else Runtime.getRuntime.availableProcessors
    }
    parse(System.getProperty("basis.dispatch.async.parallelism"))
  }
  
  /** Adds a thunk to the tail of the submission queue. */
  private[dispatch] final def pushQueue(thunk: () => _) {
    var q = null: Batch[() => _]
    do q = queue
    while (!Unsafe.compareAndSwapObject(this, QueueOffset, q, q :+ thunk))
    wakeForWork()
  }
  
  /** Removes and retuns a thunk from the head of the submission queue;
    * returns `null` if the queue is empty. */
  private[dispatch] final def pollQueue(): () => _ = {
    var q = null: Batch[() => _]
    do q = queue
    while (!q.isEmpty && !Unsafe.compareAndSwapObject(this, QueueOffset, q, q.tail))
    if (!q.isEmpty) q.head else null
  }
  
  /** Starts or resumes enough workers to reach the target level of parallelism. */
  @tailrec private[dispatch] final def wakeForWork() {
    val workingSize = working.size
    if (workingSize < parallelism) {
      val waiting = this.waiting
      if (!waiting.isEmpty && Unsafe.compareAndSwapObject(this, WaitingOffset, waiting, waiting.tail)) {
        val worker = waiting.head
        willResumeWorkerToIncreaseParallelism(worker, workingSize)
        worker.isWaiting = false
        if (worker.isParking) LockSupport.unpark(worker)
        wakeForWork()
      }
      else {
        val worker = Worker()
        willStartWorkerToIncreaseParallelism(worker, workingSize)
        var ws = null: Array[Worker]
        do ws = workers
        while (!Unsafe.compareAndSwapObject(this, WorkersOffset, ws, ws :+ worker))
        worker.start()
        wakeForWork()
      }
    }
  }
  
  /** Scans the submission queue and all worker queues. Upon finding work, if
    * the target parallelism exceeds the number of active threads, resume a
    * worker if one is waiting. */
  private[dispatch] final def scanForWork() {
    if (queue.length > 0 || workers.exists(_.queue.length > 0)) {
      val workingSize = working.size
      if (workingSize < parallelism) {
        val waiting = this.waiting
        if (!waiting.isEmpty && Unsafe.compareAndSwapObject(this, WaitingOffset, waiting, waiting.tail)) {
          val worker = waiting.head
          willResumeWorkerToIncreaseParallelism(worker, workingSize)
          worker.isWaiting = false
          if (worker.isParking) LockSupport.unpark(worker)
        }
      }
    }
  }
  
  /** Returns a unique worker identifier. */
  private[dispatch] final def getUID(): Int = {
    var n = 0
    do n = nextUID
    while (!Unsafe.compareAndSwapInt(this, NextUIDOffset, n, n + 1))
    n
  }
  
  override def apply[A](expr: => A): Relay[A] = {
    val t = new Thunk[A]
    exec(new Thunk.Eval(expr, t))
    t
  }
  
  override def block[A](expr: => A): A = {
    val thread = Thread.currentThread
    if (thread.isInstanceOf[WorkerThread]) thread.asInstanceOf[WorkerThread].block(expr)
    else expr
  }
  
  override def exec[U](thunk: () => U) {
    val thread = Thread.currentThread
    if (thread.isInstanceOf[WorkerThread]) thread.asInstanceOf[WorkerThread].pushQueue(thunk)
    else pushQueue(thunk)
  }
  
  override def relay[A](thunk: () => A): Relay[A] = {
    val t = new Thunk[A]
    exec(new Thunk.Exec(thunk, t))
    t
  }
  
  override def relayAll[A](thunks: Enumerator[() => A]): Relay[Batch[A]] = {
    val g = new Thunk.Group[A]
    thunks traverse {
      val thread = Thread.currentThread
      if (thread.isInstanceOf[WorkerThread]) new Async.RelayAllToWorker(g, thread.asInstanceOf[WorkerThread])
      else new Async.RelayAll(g, async)
    }
    g.commit()
    g
  }
  
  override def relayAny[A](thunks: Enumerator[() => A]): Relay[A] = {
    val t = new Thunk[A]
    thunks traverse {
      val thread = Thread.currentThread
      if (thread.isInstanceOf[WorkerThread]) new Async.RelayAnyToWorker(t, thread.asInstanceOf[WorkerThread])
      else new Async.RelayAny(t, async)
    }
    t
  }
  
  /** The type of worker threads.
    * @group Workers */
  protected type Worker <: WorkerThread
  
  /** Creates a new worker thread.
    * @group Workers */
  protected def Worker(): Worker
  
  /** Returns the type of worker threads.
    * @group Workers */
  protected implicit def WorkerTag: ClassTag[Worker]
  
  /** An asynchronous worker thread.
    * @group Workers */
  protected class WorkerThread extends Thread { this: Worker =>
    import MetaWorkerThread._
    
    /** The work queue for this worker. */
    @volatile private[dispatch] final var queue: Batch[() => _] = Batch.empty[() => _]
    
    /** The unique identifier of this worker. */
    final val uid: Int = getUID()
    
    /** The random seed to pick workers from whom to steal. */
    private[dispatch] final var seed: Int = System.currentTimeMillis.toInt
    
    /** Returns `true` if this worker is currently waiting. */
    @volatile private[dispatch] final var isWaiting: Boolean = false
    
    /** Returns `true` if this worker thread intends to park. */
    @volatile private[dispatch] final var isParking: Boolean = false
    
    /** Returns `true` if this worker intends to terminate. */
    @volatile private[dispatch] final var isHalting: Boolean = false
    
    setName(name)
    setDaemon(true)
    
    /** Returns `true` if this worker is active (in the working set). */
    private[dispatch] final def isWorking: Boolean = working contains this
    
    /** Adds or removes this worker from the working set. */
    private[dispatch] final def isWorking_= (is: Boolean) {
      var ws = null: HashSet[Worker]
      if (is) do ws = working
              while (!Unsafe.compareAndSwapObject(async, WorkingOffset, ws, ws + this))
      else    do ws = working
              while (!Unsafe.compareAndSwapObject(async, WorkingOffset, ws, ws - this))
    }
    
    /** Returns `true` if this worker may be blocking (in the blocked set). */
    private[dispatch] final def isBlocked: Boolean = blocked contains this
    
    /** Adds or removes this worker from the blocked set. */
    private[dispatch] final def isBlocked_= (is: Boolean) {
      var ws = null: HashSet[Worker]
      if (is) do ws = blocked
              while (!Unsafe.compareAndSwapObject(async, BlockedOffset, ws, ws + this))
      else    do ws = blocked
              while (!Unsafe.compareAndSwapObject(async, BlockedOffset, ws, ws - this))
    }
    
    /** Adds a thunk to the tail of the work queue. */
    private[dispatch] final def pushQueue(thunk: () => _) {
      var q = null: Batch[() => _]
      do q = queue
      while (!Unsafe.compareAndSwapObject(this, QueueOffset, q, q :+ thunk))
      if (q.isEmpty) wakeForWork()
    }
    
    /** Removes and retuns a thunk from the head of the work queue;
      * returns `null` if the queue is empty. */
    private[dispatch] final def pollQueue(): () => _ = {
      var q = null: Batch[() => _]
      do q = queue
      while (!q.isEmpty && !Unsafe.compareAndSwapObject(this, QueueOffset, q, q.tail))
      if (!q.isEmpty) q.head else null
    }
    
    /** Steals a thunk from the head of another worker's queue;
      * returns `null` if no work was found. */
    private[dispatch] final def pollWorkers(): () => _ = {
      val ws = workers
      var i = 0
      val n = ws.length
      var thunk = null: () => _
      while (thunk == null && i < n) {
        var r = seed
        thunk = ws(java.lang.Math.abs(r) % n).pollQueue()
        r ^= r << 13; r ^= r >>> 17; seed = r ^ r << 5
        i += 1
      }
      i = 0
      while (thunk == null && i < n) {
        thunk = ws(i).pollQueue()
        i += 1
      }
      thunk
    }
    
    /** Removes and returns a thunk from this worker's queue, or another
      * worker's queue, or the submission queue, or returns `null` if no
      * work was found. */
    private[dispatch] final def pollWork(): () => _ = {
      var thunk = pollQueue()
      if (thunk == null) {
        thunk = pollWorkers()
        if (thunk == null)
          thunk = async.pollQueue()
      }
      thunk
    }
    
    /** Evaluates an expression that potentially blocks and compensates by
      * releasing a waiting worker or starting a new worker if no active
      * workers remain. */
    private[dispatch] final def block[A](expr: => A): A = {
      isWorking = false
      isBlocked = true
      val workers = async.workers
      val working = async.working
      val waiting = async.waiting
      if (working.size < parallelism && !waiting.isEmpty &&
          Unsafe.compareAndSwapObject(async, WaitingOffset, waiting, waiting.tail)) {
        val worker = waiting.head
        willResumeWorkerToCompensateForBlock(worker, this)
        worker.isWaiting = false
        if (worker.isParking) LockSupport.unpark(worker)
      }
      else if (workers.length < parallelism || working.isEmpty) {
        val worker = Worker()
        willStartWorkerToCompensateForBlock(worker, this)
        var ws = null: Array[Worker]
        do ws = workers
        while (!Unsafe.compareAndSwapObject(async, WorkersOffset, ws, ws :+ worker))
        worker.start()
      }
      val result = expr
      isBlocked = false
      isWorking = true
      result
    }
    
    /** Runs this worker's main loop. */
    final override def run() {
      didStartWorker(this)
      isWorking = true
      try work()
      finally {
        isWorking = false
        var ws = null: Array[Worker]
        do ws = workers
        while (!Unsafe.compareAndSwapObject(async, WorkersOffset, ws, ws filter (_ ne this)))
        didStopWorker(this)
      }
    }
    
    /** Executes thunks until signalled to terminate;
      * idles when no work is available. */
    private[dispatch] final def work() {
      while (!isHalting) {
        val thunk = pollWork()
        if (thunk != null) exec(thunk)
        else idle()
      }
    }
    
    /** Executes a thunk and reports any thrown exception. */
    private[dispatch] final def exec(thunk: () => _) {
      try thunk()
      catch { case e: Exception => didThrowException(this, e) }
    }
    
    /** Parks this worker thread in between scans for work. Signals this worker
      * to terminate if the number of active threads exceeds the target parallelism. */
    private[dispatch] final def idle() {
      isWorking = false
      willPauseWorker(this)
      var ws = null: List[Worker]
      do ws = waiting
      while (!Unsafe.compareAndSwapObject(async, WaitingOffset, ws, this :: ws))
      isWaiting = true
      didPauseWorker(this)
      if (working.size == parallelism) {
        isParking = true
        LockSupport.parkNanos(async, 4000000000L)
        val workingSize = working.size
        if (isWaiting && workingSize > parallelism) {
          willStopWorkerToDecreaseParallelism(this, workingSize)
          isHalting = true
        }
        isParking = false
      }
      while (isWaiting) {
        scanForWork()
        isParking = true
        if (isWaiting) {
          Thread.interrupted()
          LockSupport.park(async)
        }
        isParking = false
      }
      didResumeWorker(this)
      isWorking = true
    }
    
    /** Returns the name of this worker. */
    protected def name: String = {
      new java.lang.StringBuilder().
        append(async.name).
        append('-').
        append("worker").
        append('-').
        append(uid).toString
    }
  }
  
  /** Returns the name of this async pool.
    * @group Classifying */
  protected def name: String = "async"
  
  /** @group Monitoring */
  protected def didThrowException(worker: Worker, cuase: Throwable): Unit = ()
  
  /** @group Monitoring */
  @elidable(1500) protected def willStartWorkerToIncreaseParallelism(worker: Worker, workingSize: Int): Unit = ()
  
  /** @group Monitoring */
  @elidable(1500) protected def willStartWorkerToCompensateForBlock(worker: Worker, blocker: Worker): Unit = ()
  
  /** @group Monitoring */
  @elidable(1500) protected def didStartWorker(worker: Worker): Unit = ()
  
  /** @group Monitoring */
  @elidable(1500) protected def willStopWorkerToDecreaseParallelism(worker: Worker, workingSize: Int): Unit = ()
  
  /** @group Monitoring */
  @elidable(1500) protected def didStopWorker(worker: Worker): Unit = ()
  
  /** @group Monitoring */
  @elidable(1500) protected def willPauseWorker(worker: Worker): Unit = ()
  
  /** @group Monitoring */
  @elidable(1500) protected def didPauseWorker(worker: Worker): Unit = ()
  
  /** @group Monitoring */
  @elidable(1500) protected def willResumeWorkerToIncreaseParallelism(worker: Worker, workingSize: Int): Unit = ()
  
  /** @group Monitoring */
  @elidable(1500) protected def willResumeWorkerToCompensateForBlock(worker: Worker, blocker: Worker): Unit = ()
  
  /** @group Monitoring */
  @elidable(1500) protected def didResumeWorker(worker: Worker): Unit = ()
}

/** Async implementations. */
private[dispatch] object Async {
  /** The primary, unmonitored async implementation. */
  object Main extends Async {
    protected final override type Worker = WorkerThread
    
    protected final override def Worker(): Worker = new Worker
    
    protected implicit final override def WorkerTag: ClassTag[Worker] = ClassTag[Worker](Predef.classOf[Worker])
  }
  
  private[dispatch] final class RelayAll[-A]
      (group: Thunk.Group[A], async: Async)
    extends AbstractFunction1[() => A, Unit] {
    override def apply(thunk: () => A) {
      group.size -= 1
      async pushQueue new group.Put(thunk)
    }
  }
  
  private[dispatch] final class RelayAllToWorker[-A]
      (group: Thunk.Group[A], worker: A#WorkerThread forSome { type A <: Async })
    extends AbstractFunction1[() => A, Unit] {
    override def apply(thunk: () => A) {
      group.size -= 1
      worker pushQueue new group.Put(thunk)
    }
  }
  
  private[dispatch] final class RelayAny[-A]
      (latch: Latch[A], async: Async)
    extends AbstractFunction1[() => A, Unit] {
    override def apply(thunk: () => A): Unit = async pushQueue new Thunk.Race(thunk, latch)
  }
  
  private[dispatch] final class RelayAnyToWorker[-A]
      (latch: Latch[A], worker: A#WorkerThread forSome { type A <: Async })
    extends AbstractFunction1[() => A, Unit] {
    override def apply(thunk: () => A): Unit = worker pushQueue new Thunk.Race(thunk, latch)
  }
}
