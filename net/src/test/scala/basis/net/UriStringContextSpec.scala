//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.net

import org.scalatest._

class UriStringContextSpec extends FlatSpec with Matchers {
  override def suiteName = "UriStringContext specification"

  "URI string substitution" should "substitute scheme variables" in {
    val scheme = Scheme("scheme")
    uri"$scheme:" should equal (Uri(scheme))
  }

  it should "substitute authority variables" in {
    val authority = Authority(Host.Name("domain"))
    uri"//$authority" should equal (Uri(authority = authority))
  }

  it should "substitute user info variables" in {
    val userInfo = UserInfo("user", "pass")
    uri"//$userInfo@domain" should equal (
      Uri(authority = Authority(Host.Name("domain"), userInfo = userInfo)))
  }

  it should "substitute host variables" in {
    val host = Host.Name("domain")
    uri"//$host" should equal (Uri(authority = Authority(host)))
  }

  it should "substitute port variables" in {
    val port = Port(80)
    uri"//domain:$port" should equal (
      Uri(authority = Authority(Host.Name("domain"), port)))
  }

  it should "substitute user info and host variables" in {
    val userInfo = UserInfo("user", "pass")
    val host = Host.Name("domain")
    uri"//$userInfo@$host" should equal (
      Uri(authority = Authority(host, userInfo = userInfo)))
  }

  it should "substitute host and port variables" in {
    val host = Host.Name("domain")
    val port = Port(80)
    uri"//$host:$port" should equal (Uri(authority = Authority(host, port)))
  }

  it should "substitute user info, host, and port variables" in {
    val userInfo = UserInfo("user", "pass")
    val host = Host.Name("domain")
    val port = Port(80)
    uri"//$userInfo@$host:$port" should equal (
      Uri(authority = Authority(host, port, userInfo)))
  }

  it should "substitute path variables" in {
    val path = Path.slash
    uri"$path" should equal (Uri(path = path))
  }

  it should "substitute subpath variables" in {
    val one = Path("one")
    val two = Path("two")
    uri"/$one" should equal (Uri(path = Path / "one"))
    uri"/$one/" should equal (Uri(path = Path / "one" /))
    uri"/one/$two" should equal (Uri(path = Path / "one" / "two"))
    uri"/one/$two/" should equal (Uri(path = Path / "one" / "two" /))
    uri"one/$two" should equal (Uri(path = Path("one") / "two"))
    uri"one/$two/" should equal (Uri(path = Path("one") / "two" /))
    uri"/$one/$two" should equal (Uri(path = Path / "one" / "two"))
    uri"/$one/$two/" should equal (Uri(path = Path / "one" / "two" /))
    uri"$one/$two" should equal (Uri(path = Path("one") / "two"))
    uri"$one/$two/" should equal (Uri(path = Path("one") / "two" /))
  }

  it should "substitute query variables" in {
    val query = Query("query")
    uri"?$query" should equal (Uri(query = query))
  }

  it should "substitute fragment variables" in {
    val fragment = Fragment("fragment")
    uri"#$fragment" should equal (Uri(fragment = fragment))
  }

  it should "substitute scheme and authority variables" in {
    val scheme = Scheme("scheme")
    val authority = Authority(Host.Name("domain"))
    uri"$scheme://$authority" should equal (Uri(scheme, authority))
  }

  it should "substitute scheme and path variables" in {
    val scheme = Scheme("scheme")
    val path = Path("path")
    uri"$scheme:$path" should equal (Uri(scheme, path = path))
  }

  it should "substitute scheme and query variables" in {
    val scheme = Scheme("scheme")
    val query = Query("query")
    uri"$scheme:?$query" should equal (Uri(scheme, query = query))
  }

  it should "substitute scheme and fragment variables" in {
    val scheme = Scheme("scheme")
    val fragment = Fragment("fragment")
    uri"$scheme:#$fragment" should equal (Uri(scheme, fragment = fragment))
  }

  it should "substitute scheme, authority, and path variables" in {
    val scheme = Scheme("scheme")
    val authority = Authority(Host.Name("domain"))
    val path = Path("path")
    uri"$scheme://$authority$path" should equal (Uri(scheme, authority, path))
  }

  it should "substitute scheme, authority, and query variables" in {
    val scheme = Scheme("scheme")
    val authority = Authority(Host.Name("domain"))
    val query = Query("query")
    uri"$scheme://$authority?$query" should equal (Uri(scheme, authority, query = query))
  }

  it should "substitute scheme, authority, and fragment variables" in {
    val scheme = Scheme("scheme")
    val authority = Authority(Host.Name("domain"))
    val fragment = Fragment("fragment")
    uri"$scheme://$authority#$fragment" should equal (Uri(scheme, authority, fragment = fragment))
  }

  it should "substitute scheme, authority, path, and query variables" in {
    val scheme = Scheme("scheme")
    val authority = Authority(Host.Name("domain"))
    val path = Path("path")
    val query = Query("query")
    uri"$scheme://$authority$path?$query" should equal (Uri(scheme, authority, path, query))
  }

  it should "substitute scheme, authority, path, and fragment variables" in {
    val scheme = Scheme("scheme")
    val authority = Authority(Host.Name("domain"))
    val path = Path("path")
    val fragment = Fragment("fragment")
    uri"$scheme://$authority$path#$fragment" should equal (Uri(scheme, authority, path, fragment = fragment))
  }

  it should "substitute scheme, authority, path, query, and fragment variables" in {
    val scheme = Scheme("scheme")
    val authority = Authority(Host.Name("domain"))
    val path = Path("path")
    val query = Query("query")
    val fragment = Fragment("fragment")
    uri"$scheme://$authority$path?$query#$fragment" should equal (Uri(scheme, authority, path, query, fragment))
  }
}
