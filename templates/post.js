module.exports = {
  html(date, body) {
    const formattedDate = (new Date(date)).toLocaleDateString(
      "en-US",
      { year: "numeric", month: "long", day: "numeric" }
    );

    return `<article>
  <header>
    <p class="date">${formattedDate}</p>
  </header>
  <section>
    ${body}
  </section>
</article>`;
  }
};
