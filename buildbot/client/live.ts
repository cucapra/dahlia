async function startJob(code: string): Promise<string> {
  const data = new FormData();
  data.append('code', code);
  const res = await fetch("/jobs", {
      method: "POST",
      body: data,
  });
  return await res.text();
}

document.addEventListener("DOMContentLoaded", () => {
  const textarea = document.querySelector("textarea");
  const button = document.querySelector("button");

  async function compile() {
    const code = textarea.value;
    console.log("compiling", code);

    const jobName = await startJob(code);
    console.log(jobName);
  }

  button.addEventListener("click", compile);
});
