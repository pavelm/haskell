<html>
  <head>
    <title>Snap web server</title>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" type="text/css" href="/bootstrap-3.1.1-dist/css/bootstrap.min.css">
    <link rel="icon" type="image/png" href="/static/favicon.png">
  </head>
  <body>
    <div class="container">
      <div class="navbar navbar-default" role="navigation">
          <div class="container-fluid">
              <div class="navbar-header">
                <button type="button" class="navbar-toggle" data-toggle="collapse" data-targets="bs-example-navbar-collapse-1">
                    <span class="sr-only">Toggle navigation</span>
                    <span class="icon-bar"/>
                    <span class="icon-bar"/>
                    <span class="icon-bar"/>
                </button>
                <a class="navbar-brand" href="/">Linx</a>
              </div>
              <div class="navbar-collapse collapse">
                  <ul class="nav navbar-nav navbar-right">
                      <ifLoggedIn>
                          <li>Hello <loggedInUser/></li>
                      </ifLoggedIn>
                  </ul>
              </div>
          </div>
      </div>
      <div class="jumbotron">
        <div id="content">
          <apply-content/>
        </div>
      </div>
    </div>
  </body>
</html>
