module.exports = {
  site: "giraffedev",
  html(title, body) {
    return `\
<!doctype html>
<html lang="en">
    <head>
        <meta charset="utf-8">
        <meta http-equiv="x-ua-compatible" content="ie=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <title>${this.site} - ${title}</title>
        <link rel="stylesheet" href="/css/default.css" />
    </head>
    <body class="container">
      <header>
        <h2 class="header">
          <a class="header__site link" href="/">${this.site}:</a> <span class="header__post">${title}</span>
        </h2>
      </header>

      <main class="content" role="main">
        ${body}
      </main>
      <footer>
        <ul class="footer">
          <li class="footer__item"><a class="link" href="/archive">archive</a></li>
          <li class="footer__item"><a class="link" href="/contact">contact</a></li>
          <li class="footer__item"><a class="link" href="https://github.com/emccorson/giraffedev-site" target="_blank">source</a></li>
        </ul>
      </footer>
    </body>
</html>`;
  }
};
