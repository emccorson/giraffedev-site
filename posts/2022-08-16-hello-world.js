module.exports = {
  title: "hello, world",
  date: "2024-07-14T21:26:29+09:00",
  html() {
    return `
<section>
  <pre><code>main : IO ()
main = putStrLn "hello, world"</code></pre>
</section>
`;
  }
}
