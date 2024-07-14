const { titleToUrl, mostRecentDateSort } = require("../utils.js");

const dateGroup = (groupFn, posts) => posts.reduce(
  (acc, curr) => {
    const index = groupFn.apply(new Date(curr.date));

    return { ...acc, [index]: [ ...(acc[index] || []), curr ] };
  },
  {}
);

const postFragment = (post) => `<tr>
  <th>${(new Date(post.date)).getDate()}</th>
  <td><a class="link" href="${titleToUrl(post.title)}">${post.title}</a></td>
</tr>
`;

const monthFragment = (month, posts) => {
  const formattedMonth = (() => {
    const date = new Date();
    date.setMonth(month);
    return date.toLocaleString("en-US", { month: "long" });
  })();

  const body = posts
    .toSorted(mostRecentDateSort)
    .map(postFragment)
    .join("");

  return `<tr>
  <th>${formattedMonth}</th>
</tr>
${body}
`;
};

const yearFragment = (year, posts) => {
  const monthGrouped = dateGroup(Date.prototype.getMonth, posts);

  const body = Object.entries(monthGrouped)
    .toSorted((a, b) => Number(b[0]) - Number(a[0]))
    .map(([month, posts]) => monthFragment(month, posts))
    .join("");

  return `<tr class="archive__year">
  <th>${year}</th>
</tr>

${body}`;
}

module.exports = {
  html(posts) {
    const yearGrouped = dateGroup(Date.prototype.getFullYear, posts);

    const body = Object.entries(yearGrouped)
      .toSorted((a, b) => Number(b[0]) - Number(a[0]))
      .map(([year, posts]) => yearFragment(year, posts))
      .join("");

    return `<table class="archive">${body}</table>`;
  }
};
