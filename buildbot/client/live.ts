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
  const jobLink = document.querySelector("#jobLink");

  async function compile() {
    const code = textarea.value;
    console.log("compiling", code);

    const jobName = await startJob(code);
    console.log("started job", jobName);
    result.textContent = 'job started...';
    jobLink.textContent = jobName;
    jobLink.setAttribute("href", "/jobs/" + jobName + ".html");

    const poll = async () => {
      const res = await fetch("/jobs/" + jobName);
      const job = await res.json();
      console.log("updated job", job);

      if (job.state === "failed") {
        result.textContent = 'failed!';
      } else if (job.c_main) {
        console.log('compiled');
        const res = await fetch("/jobs/" + jobName + "/files/code/" + job.c_main);
        const c_code = await res.text();
        result.textContent = c_code;
      } else {
        setTimeout(poll, 1000);
      }
    };

    setTimeout(poll, 100);
  }

  button.addEventListener("click", compile);
});
