//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2015 Chris Sachs
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis.net

import org.scalatest._

class UriSpec extends FlatSpec with Matchers {
  override def suiteName = "Uri specification"

  "URI parser" should "parse empty URIs" in {
    Uri("") should equal (Uri.empty)
  }

  it should "parse URIs with schemes" in {
    Uri("scheme:") should equal (Uri(scheme = "scheme"))
    Uri("AZaz09+-.:") should equal (Uri(scheme = "AZaz09+-."))
  }

  it should "parse URIs with empty authorities" in {
    Uri("//") should equal (Uri.empty)
  }

  it should "parse URIs with host names" in {
    Uri("//domain") should equal (Uri(authority = Authority(Host.Name("domain"))))
  }

  it should "parse URIs with IPv4 addresses" in {
    Uri("//127.0.0.1") should equal (Uri(authority = Authority(Host.IPv4("127.0.0.1"))))
  }

  it should "parse URIs with IPv6 addresses" in {
    Uri("//[::1]") should equal (Uri(authority = Authority(Host.IPv6("::1"))))
  }

  it should "parse URIs with host names and ports" in {
    Uri("//domain:80") should equal (
      Uri(authority = Authority(Host.Name("domain"), Port(80))))
  }

  it should "parse URIs with IPv4 addresses and ports" in {
    Uri("//127.0.0.1:80") should equal (
      Uri(authority = Authority(Host.IPv4("127.0.0.1"), Port(80))))
  }

  it should "parse URIs with IPv6 addresses and ports" in {
    Uri("//[::1]:80") should equal (
      Uri(authority = Authority(host = Host.IPv6("::1"), Port(80))))
  }

  it should "parse URIs with user info and host names" in {
    Uri("//user:pass@domain") should equal (
      Uri(authority = Authority(Host.Name("domain"), userInfo = UserInfo("user", "pass"))))
  }

  it should "parse URIs with user info and IPv4 addresses" in {
    Uri("//user:pass@127.0.0.1") should equal (
      Uri(authority = Authority(Host.IPv4("127.0.0.1"), userInfo = UserInfo("user", "pass"))))
  }

  it should "parse URIs with user info and IPv6 addresses" in {
    Uri("//user:pass@[::1]") should equal (
      Uri(authority = Authority(Host.IPv6("::1"), userInfo = UserInfo("user", "pass"))))
  }

  it should "parse URIs with user info, host names, and ports" in {
    Uri("//user:pass@domain:80") should equal (
      Uri(authority = Authority(Host.Name("domain"), Port(80), UserInfo("user", "pass"))))
  }

  it should "parse URIs with user info, IPv4 addresses, and ports" in {
    Uri("//user:pass@127.0.0.1:80") should equal (
      Uri(authority = Authority(Host.IPv4("127.0.0.1"), Port(80), UserInfo("user", "pass"))))
  }

  it should "parse URIs with user info, IPv6 addresses, and ports" in {
    Uri("//user:pass@[::1]:80") should equal (
      Uri(authority = Authority(Host.IPv6("::1"), Port(80), UserInfo("user", "pass"))))
  }

  it should "parse URIs with absolute paths" in {
    Uri("/") should equal (Uri(path = Path /))
    Uri("/one") should equal (Uri(path = Path / "one"))
    Uri("/one/") should equal (Uri(path = Path / "one" /))
    Uri("/one/two") should equal (Uri(path = Path / "one" / "two"))
    Uri("/one/two/") should equal (Uri(path = Path / "one" / "two" /))
  }

  it should "parse URIs with relative paths" in {
    Uri("one") should equal (Uri(path = Path("one")))
    Uri("one/") should equal (Uri(path = Path("one") /))
    Uri("one/two") should equal (Uri(path = Path("one") / "two"))
    Uri("one/two/") should equal (Uri(path = Path("one") / "two" /))
  }

  it should "parse URIs with paths containing permitted deliminters" in {
    Uri("/one/!$&()*+,;='/three") should equal (Uri(path = Path / "one" / "!$&()*+,;='" / "three"))
  }

  it should "parse URIs with paths beginning with percent escapes" in {
    Uri("%20") should equal(Uri(path = Path.Part(" ")))
  }

  it should "parse URIs with empty queries" in {
    Uri("?") should equal (Uri(query = Query.Part("")))
  }

  it should "parse URIs with query parts" in {
    Uri("?query") should equal (Uri(query = Query.Part("query")))
  }

  it should "parse URIs with query params" in {
    Uri("?key=value") should equal (Uri(query = Query :+ ("key", "value")))
    Uri("?k1=v1&k2=v2") should equal (Uri(query = Query :+ ("k1", "v1") :+ ("k2", "v2")))
    Uri("?k1=v=1") should equal (Uri(query = Query :+ ("k1", "v=1")))
    Uri("?k1=") should equal (Uri(query = Query :+ ("k1", "")))
    Uri("?=v1") should equal (Uri(query = Query :+ ("", "v1")))
    Uri("?=") should equal (Uri(query = Query :+ ("", "")))
    Uri("?a&b") should equal (Uri(query = Query :+ ("", "a") :+ ("", "b")))
  }

  it should "parse URIs with queries containing permitted delimiters" in {
    Uri("?!$()*+,/:;?@'") should equal (Uri(query = Query.Part("!$()*+,/:;?@'")))
  }

  it should "parse URIs with empty fragments" in {
    Uri("#") should equal (Uri(fragment = Fragment.Part("")))
  }

  it should "parse URIs with fragments" in {
    Uri("#fragment") should equal (Uri(fragment = Fragment.Part("fragment")))
  }

  it should "parse URIs with fragments containing permitted delimiters" in {
    Uri("#!$&()*+,/:;?@='") should equal (Uri(fragment = Fragment.Part("!$&()*+,/:;?@='")))
  }

  it should "parse URIs with schemes and authorities" in {
    Uri("scheme://domain") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"))))
    Uri("scheme://127.0.0.1") should equal (
      Uri(Scheme("scheme"), Authority(Host.IPv4("127.0.0.1"))))
    Uri("scheme://[::1]") should equal (
      Uri(Scheme("scheme"), Authority(Host.IPv6("::1"))))
    Uri("scheme://domain:80") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"), Port(80))))
    Uri("scheme://127.0.0.1:80") should equal (
      Uri(Scheme("scheme"), Authority(Host.IPv4("127.0.0.1"), Port(80))))
    Uri("scheme://[::1]:80") should equal (
      Uri(Scheme("scheme"), Authority(Host.IPv6("::1"), Port(80))))
    Uri("scheme://user:pass@domain") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"), userInfo = UserInfo("user", "pass"))))
    Uri("scheme://user:pass@127.0.0.1") should equal (
      Uri(Scheme("scheme"), Authority(Host.IPv4("127.0.0.1"), userInfo = UserInfo("user", "pass"))))
    Uri("scheme://user:pass@[::1]") should equal (
      Uri(Scheme("scheme"), Authority(Host.IPv6("::1"), userInfo = UserInfo("user", "pass"))))
    Uri("scheme://user:pass@domain:80") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"), Port(80), UserInfo("user", "pass"))))
    Uri("scheme://user:pass@127.0.0.1:80") should equal (
      Uri(Scheme("scheme"), Authority(Host.IPv4("127.0.0.1"), Port(80), UserInfo("user", "pass"))))
    Uri("scheme://user:pass@[::1]:80") should equal (
      Uri(Scheme("scheme"), Authority(Host.IPv6("::1"), Port(80), UserInfo("user", "pass"))))
  }

  it should "parse URIs with schemes and absolute paths" in {
    Uri("scheme:/") should equal (Uri(Scheme("scheme"), path = Path /))
    Uri("scheme:/one") should equal (Uri(Scheme("scheme"), path = Path / "one"))
    Uri("scheme:/one/") should equal (Uri(Scheme("scheme"), path = Path / "one" /))
    Uri("scheme:/one/two") should equal (Uri(Scheme("scheme"), path = Path / "one" / "two"))
    Uri("scheme:/one/two/") should equal (Uri(Scheme("scheme"), path = Path / "one" / "two" /))
  }

  it should "parse URIs with schemes and relative paths" in {
    Uri("scheme:one") should equal (Uri(Scheme("scheme"), path = Path("one")))
    Uri("scheme:one/") should equal (Uri(Scheme("scheme"), path = Path("one") /))
    Uri("scheme:one/two") should equal (Uri(Scheme("scheme"), path = Path("one") / "two"))
    Uri("scheme:one/two/") should equal (Uri(Scheme("scheme"), path = Path("one") / "two" /))
  }

  it should "parse URIs with schemes and queries" in {
    Uri("scheme:?query") should equal (Uri(Scheme("scheme"), query = Query.Part("query")))
    Uri("scheme:?key=value") should equal (Uri(Scheme("scheme"), query = Query :+ ("key", "value")))
  }

  it should "parse URIs with schemes and fragments" in {
    Uri("scheme:#fragment") should equal (Uri(Scheme("scheme"), fragment = Fragment.Part("fragment")))
  }

  it should "parse URIs with schemes, authorities, and paths" in {
    Uri("scheme://domain/path") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain")), Path / "path"))
    Uri("scheme://127.0.0.1/path") should equal (
      Uri(Scheme("scheme"), Authority(Host.IPv4("127.0.0.1")), Path / "path"))
    Uri("scheme://[::1]/path") should equal (
      Uri(Scheme("scheme"), Authority(Host.IPv6("::1")), Path / "path"))
    Uri("scheme://domain:80/path") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"), Port(80)), Path / "path"))
    Uri("scheme://127.0.0.1:80/path") should equal (
      Uri(Scheme("scheme"), Authority(Host.IPv4("127.0.0.1"), Port(80)), Path / "path"))
    Uri("scheme://[::1]:80/path") should equal (
      Uri(Scheme("scheme"), Authority(Host.IPv6("::1"), Port(80)), Path / "path"))
    Uri("scheme://user@domain/path") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"), userInfo = UserInfo("user")), Path / "path"))
    Uri("scheme://user@127.0.0.1/path") should equal (
      Uri(Scheme("scheme"), Authority(Host.IPv4("127.0.0.1"), userInfo = UserInfo("user")), Path / "path"))
    Uri("scheme://user@[::1]/path") should equal (
      Uri(Scheme("scheme"), Authority(Host.IPv6("::1"), userInfo = UserInfo("user")), Path / "path"))
    Uri("scheme://user@domain:80/path") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"), Port(80), UserInfo("user")), Path / "path"))
    Uri("scheme://user@127.0.0.1:80/path") should equal (
      Uri(Scheme("scheme"), Authority(Host.IPv4("127.0.0.1"), Port(80), UserInfo("user")), Path / "path"))
    Uri("scheme://user@[::1]:80/path") should equal (
      Uri(Scheme("scheme"), Authority(Host.IPv6("::1"), Port(80), UserInfo("user")), Path / "path"))
  }

  it should "parse URIs with schemes, authorities, and queries" in {
    Uri("scheme://domain?query") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain")), query = Query.Part("query")))
    Uri("scheme://domain:80?query") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"), Port(80)), query = Query.Part("query")))
    Uri("scheme://user@domain?query") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"), userInfo = UserInfo("user")), query = Query.Part("query")))
    Uri("scheme://user@domain:80?query") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"), Port(80), UserInfo("user")), query = Query.Part("query")))
  }

  it should "parse URIs with schemes, authorities, and fragments" in {
    Uri("scheme://domain#fragment") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain")), fragment = Fragment.Part("fragment")))
    Uri("scheme://domain:80#fragment") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"), Port(80)), fragment = Fragment.Part("fragment")))
    Uri("scheme://user@domain#fragment") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"), userInfo = UserInfo("user")), fragment = Fragment.Part("fragment")))
    Uri("scheme://user@domain:80#fragment") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"), Port(80), UserInfo("user")), fragment = Fragment.Part("fragment")))
  }

  it should "parse URIs with schemes, authorities, paths, and queries" in {
    Uri("scheme://domain/path?query") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain")), Path / "path", Query.Part("query")))
    Uri("scheme://domain:80/path?query") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"), Port(80)), Path / "path", Query.Part("query")))
    Uri("scheme://user@domain/path?query") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"), userInfo = UserInfo("user")), Path / "path", Query.Part("query")))
    Uri("scheme://user@domain:80/path?query") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"), Port(80), UserInfo("user")), Path / "path", Query.Part("query")))
  }

  it should "parse URIs with schemes, authorities, paths, and fragments" in {
    Uri("scheme://domain/path#fragment") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain")), Path / "path", fragment = Fragment.Part("fragment")))
    Uri("scheme://domain:80/path#fragment") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"), Port(80)), Path / "path", fragment = Fragment.Part("fragment")))
    Uri("scheme://user@domain/path#fragment") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"), userInfo = UserInfo("user")), Path / "path", fragment = Fragment.Part("fragment")))
    Uri("scheme://user@domain:80/path#fragment") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"), Port(80), UserInfo("user")), Path / "path", fragment = Fragment.Part("fragment")))
  }

  it should "parse URIs with schemes, authorities, paths, queries, and fragments" in {
    Uri("scheme://domain/path?query#fragment") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain")), Path / "path", Query.Part("query"), Fragment.Part("fragment")))
    Uri("scheme://domain:80/path?query#fragment") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"), Port(80)), Path / "path", Query.Part("query"), Fragment.Part("fragment")))
    Uri("scheme://user@domain/path?query#fragment") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"), userInfo = UserInfo("user")), Path / "path", Query.Part("query"), Fragment.Part("fragment")))
    Uri("scheme://user@domain:80/path?query#fragment") should equal (
      Uri(Scheme("scheme"), Authority(Host.Name("domain"), Port(80), UserInfo("user")), Path / "path", Query.Part("query"), Fragment.Part("fragment")))
  }


  "Relative URI resolution" should "resolve normal URI references" in {
    val base = uri"http://a/b/c/d;p?q"
    base.resolve(uri"g:h") should equal (uri"g:h")
    base.resolve(uri"g") should equal (uri"http://a/b/c/g")
    base.resolve(uri"./g") should equal (uri"http://a/b/c/g")
    base.resolve(uri"g/") should equal (uri"http://a/b/c/g/")
    base.resolve(uri"/g") should equal (uri"http://a/g")
    base.resolve(uri"//g") should equal (uri"http://g")
    base.resolve(uri"?y") should equal (uri"http://a/b/c/d;p?y")
    base.resolve(uri"g?y") should equal (uri"http://a/b/c/g?y")
    base.resolve(uri"#s") should equal (uri"http://a/b/c/d;p?q#s")
    base.resolve(uri"g#s") should equal (uri"http://a/b/c/g#s")
    base.resolve(uri"g?y#s") should equal (uri"http://a/b/c/g?y#s")
    base.resolve(uri";x") should equal (uri"http://a/b/c/;x")
    base.resolve(uri"g;x") should equal (uri"http://a/b/c/g;x")
    base.resolve(uri"g;x?y#s") should equal (uri"http://a/b/c/g;x?y#s")
    base.resolve(uri"") should equal (uri"http://a/b/c/d;p?q")
    base.resolve(uri".") should equal (uri"http://a/b/c/")
    base.resolve(uri"./") should equal (uri"http://a/b/c/")
    base.resolve(uri"..") should equal (uri"http://a/b/")
    base.resolve(uri"../") should equal (uri"http://a/b/")
    base.resolve(uri"../g") should equal (uri"http://a/b/g")
    base.resolve(uri"../..") should equal (uri"http://a/")
    base.resolve(uri"../../") should equal (uri"http://a/")
    base.resolve(uri"../../g") should equal (uri"http://a/g")
  }

  it should "resolve abnormal URI references" in {
    val base = uri"http://a/b/c/d;p?q"

    base.resolve(uri"../../../g") should equal (uri"http://a/g")
    base.resolve(uri"../../../../g") should equal (uri"http://a/g")

    base.resolve(uri"/./g") should equal (uri"http://a/g")
    base.resolve(uri"/../g") should equal (uri"http://a/g")
    base.resolve(uri"g.") should equal (uri"http://a/b/c/g.")
    base.resolve(uri".g") should equal (uri"http://a/b/c/.g")
    base.resolve(uri"g..") should equal (uri"http://a/b/c/g..")
    base.resolve(uri"..g") should equal (uri"http://a/b/c/..g")

    base.resolve(uri"./../g") should equal (uri"http://a/b/g")
    base.resolve(uri"./g/.") should equal (uri"http://a/b/c/g/")
    base.resolve(uri"g/./h") should equal (uri"http://a/b/c/g/h")
    base.resolve(uri"g/../h") should equal (uri"http://a/b/c/h")
    base.resolve(uri"g;x=1/./y") should equal (uri"http://a/b/c/g;x=1/y")
    base.resolve(uri"g;x=1/../y") should equal (uri"http://a/b/c/y")

    base.resolve(uri"g?y/./x") should equal (uri"http://a/b/c/g?y/./x")
    base.resolve(uri"g?y/../x") should equal (uri"http://a/b/c/g?y/../x")
    base.resolve(uri"g#s/./x") should equal (uri"http://a/b/c/g#s/./x")
    base.resolve(uri"g#s/../x") should equal (uri"http://a/b/c/g#s/../x")
  }

  it should "unresolve related URIs" in {
    uri"http://example.com".unresolve(uri"http://example.com") should equal (Uri.empty)
    uri"http://example.com".unresolve(uri"http://example.com/") should equal (uri"/")
    uri"http://example.com".unresolve(uri"http://example.com/path") should equal (uri"path")
    uri"http://example.com".unresolve(uri"http://example.com?query") should equal (uri"?query")
    uri"http://example.com".unresolve(uri"http://example.com#fragment") should equal (uri"#fragment")
    uri"http://example.com/".unresolve(uri"http://example.com") should equal (uri"/")
    uri"http://example.com/".unresolve(uri"http://example.com/") should equal (Uri.empty)
    uri"http://example.com/".unresolve(uri"http://example.com/path") should equal (uri"path")
    uri"http://example.com/".unresolve(uri"http://example.com?query") should equal (uri"/?query")
    uri"http://example.com/".unresolve(uri"http://example.com#fragment") should equal (uri"/#fragment")
    uri"http://example.com/base".unresolve(uri"http://example.com") should equal (uri"/")
    uri"http://example.com/base".unresolve(uri"http://example.com/") should equal (uri"/")
    uri"http://example.com/base".unresolve(uri"http://example.com/path") should equal (uri"path")
    uri"http://example.com/base".unresolve(uri"http://example.com?query") should equal (uri"/?query")
    uri"http://example.com/base".unresolve(uri"http://example.com#fragment") should equal (uri"/#fragment")
    uri"http://example.com/base".unresolve(uri"http://example.com/base") should equal (Uri.empty)
    uri"http://example.com/base".unresolve(uri"http://example.com/base/") should equal (uri"/")
    uri"http://example.com/base".unresolve(uri"http://example.com/base/path") should equal (uri"path")
    uri"http://example.com/base".unresolve(uri"http://example.com/base?query") should equal (uri"?query")
    uri"http://example.com/base".unresolve(uri"http://example.com/base#fragment") should equal (uri"#fragment")
    uri"http://example.com/base/".unresolve(uri"http://example.com/base") should equal (uri"/base")
    uri"http://example.com/base/".unresolve(uri"http://example.com/base/") should equal (Uri.empty)
    uri"http://example.com/base/".unresolve(uri"http://example.com/base/path") should equal (uri"path")
    uri"http://example.com/base/".unresolve(uri"http://example.com/base?query") should equal (uri"/base?query")
    uri"http://example.com/base/".unresolve(uri"http://example.com/base#fragment") should equal (uri"/base#fragment")
  }

  it should "unresolve unrelated URIs" in {
    uri"http://example.com".unresolve(uri"https://example.com") should equal (uri"https://example.com")
    uri"http://example.com".unresolve(uri"http://example.org") should equal (uri"http://example.org")
  }
}
