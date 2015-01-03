subtitler
=========

Example of `application.conf`:
```
watchDirs = ["/home/pyppe/series", "/mnt/media/some-dir"]
credentials {
  openSubtitles {
    login = "my-login-to-opensubtitles.org"
    password = "my-pass"
    useragent = "my-user-agent" // OSTestUserAgent for testing, see http://trac.opensubtitles.org/projects/opensubtitles/wiki/DevReadFirst
  }
}
```
