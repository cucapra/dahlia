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
  const result = document.querySelector("#result");

  async function compile() {
    const code = textarea.value;
    console.log("compiling", code);

    const jobName = await startJob(code);
    console.log("started job", jobName);
    result.textContent = 'job started...';

    const poll = async () => {
      const res = await fetch("/jobs/" + jobName);
      const job = await res.json();
      console.log("updated job", job);

      if (job.state === "failed") {
        result.textContent = 'failed!';
      }
    };

    setTimeout(poll, 100);
  }

  button.addEventListener("click", compile);
});
