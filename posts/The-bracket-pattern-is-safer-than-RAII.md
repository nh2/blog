# DRAFT

I just encountered another typical Go bug created by Go's defer `file.Close()` pattern: https://github.com/tus/tusd/issues/698
 
This made me think of Michael Snoyman's post ["RAII is better than the bracket pattern"](https://www.snoyman.com/blog/2018/10/raii-better-than-bracket-pattern/) and I now have some doubts whether RAII is really better than the bracket pattern. In particular if the destruction function can error, and the RAII system has no way to propagate that error (e.g. exceptions).
 
In my linked issue, a file `Open()` has to be paired with a finalizing `Close()`. But `Close()` can return an error; if it's not handled, data loss will occur (e.g. by a server telling a client uploading a file was saved, when in fact it wasn't).
 
Go's `defer file.Close()` discards the returned error values.
 
C++ forbids throwing exceptions in destructors, so its RAII will swallow the error as well.
 
Python uses the bracket pattern: `with open("myfile", "w") as f: ...`. If the `close()` embedded in the `with` throws, `with` will re-throw that error. So an HTTP handler in which this with sits will fail-safe.
 
So I'd claim that the bracket pattern is better than RAII if the finalising actions can themselves fail.
