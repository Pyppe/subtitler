subtitler
=========

[![Build Status](https://travis-ci.org/Pyppe/subtitler.svg)](https://travis-ci.org/Pyppe/subtitler)

Example of `application.conf`:
```
watchDirs = ["/home/pyppe/series", "/mnt/media/some-dir"]
languages = ["eng", "fin"]
credentials {
  openSubtitles {
    login = "my-login-to-opensubtitles.org"
    password = "my-pass"
    useragent = "my-user-agent" // OSTestUserAgent for testing, see http://trac.opensubtitles.org/projects/opensubtitles/wiki/DevReadFirst
  }
}
```
