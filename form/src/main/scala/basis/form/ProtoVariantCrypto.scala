//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.form

import basis.data._
import org.bouncycastle.crypto._
import org.bouncycastle.crypto.engines._
import org.bouncycastle.crypto.modes._
import org.bouncycastle.crypto.params._

private[form] final class ProtoVariantCrypto[V <: ProtoVariant](val Variant: V) {
  import Variant._

  def encrypt(form: AnyForm, secretKey: Loader, iv: Loader): AnyForm = {
    val key = new KeyParameter(secretKey.toArray)
    val params = new AEADParameters(key, 128, iv.toArray)

    val engine = new AESEngine
    engine.init(true, key)

    val cipher = new GCMBlockCipher(engine)
    cipher.init(true, params)

    val plaintext = form.toProto.toArray
    val ciphertext = new Array[Byte](cipher.getOutputSize(plaintext.length))

    val i = cipher.processBytes(plaintext, 0, plaintext.length, ciphertext, 0)
    cipher.doFinal(ciphertext, i)

    val data = new Array[Byte](ciphertext.length - 16)
    val mac = new Array[Byte](16)
    java.lang.System.arraycopy(ciphertext, 0, data, 0, data.length)
    java.lang.System.arraycopy(ciphertext, data.length, mac, 0, 16)

    SecretForm(ArrayDataLE(data), iv, ArrayDataLE(mac))
  }

  def decrypt(secret: SecretForm, secretKey: Loader): AnyForm = try {
    val key = new KeyParameter(secretKey.toArray)
    val params = new AEADParameters(key, 128, secret.iv.toArray)

    val engine = new AESEngine
    engine.init(false, key)

    val cipher = new GCMBlockCipher(engine)
    cipher.init(false, params)

    val ciphertext = new Array[Byte](secret.data.size.toInt + secret.mac.size.toInt)
    val plaintext = new Array[Byte](cipher.getOutputSize(ciphertext.length))
    java.lang.System.arraycopy(secret.data.toArray, 0, ciphertext, 0, secret.data.size.toInt)
    java.lang.System.arraycopy(secret.mac.toArray, 0, ciphertext, secret.data.size.toInt, secret.mac.size.toInt)

    val i = cipher.processBytes(ciphertext, 0, ciphertext.length, plaintext, 0)
    cipher.doFinal(plaintext, i)

    AnyForm.readProto(ArrayDataLE(plaintext).reader(0L))
  }
  catch { case _: InvalidCipherTextException => secret }
}
