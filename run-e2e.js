const child_process = require("child_process");
const { promisify } = require("util");
const { main: e2eMain } = require('./e2e/index.js');

const exec = promisify(child_process.exec);

async function main() {
  console.log('Building...');
  const [serverBuild, uiBuild] = await Promise.all([
    exec('stack build'),
    exec('elm-app build', { cwd: 'frontend' })
  ]);
  console.log('stack build:\n----------\n');
  console.log(serverBuild.stdout);
  console.log('elm-app build:\n----------\n');
  console.log(uiBuild.stdout);

  console.log('Starting server...');
  const serverP = child_process.spawn('stack', ['run']);
  let serverLogs = '';
  serverP.stdout.on('data', data => serverLogs += data);

  await (new Promise(resolve => setTimeout(resolve, 1000)));

  console.log('Running e2e...');
  let exception = false;
  try {
    await e2eMain();
  } catch (e) {
    exception = e;
  }

  console.log('Done.');
  const stopped = serverP.kill();
  if (!stopped) {
    console.error('Something went wrong, could not stop server...');
  } else {
    console.log('Stopped server');
  }

  if (exception) {
    console.error(exception);
    process.exit(1);
  } else {
    console.log('Succeeded!');
  }
}

main();