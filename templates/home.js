module.exports = {
  html(latestPostHtml) {
    return `${latestPostHtml}

<footer>
  <a class="link link--no-hover" href="/archive">See all posts »</a>
</footer>`;
  }
};
