<img src="logo.png" alt="cas" id="logo" width="300px">

[clojure|couch] [access|auth] [server|service]
--------------------------------------------

* html pages in couch `_attachments`
* users in `_users` database (registration)
* add user as member to database (login)
* get session cookie and add to response (login) 
* redirect to login if session is timed out
* provide `index.html`
* provide `_attachments/js`, `_attachments/css` (todo)

## view documentation

https://wactbprot.github.io/cas/

## generate documentation

```shell
clojure -M:docs
```

