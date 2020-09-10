const puppeteer = require("puppeteer");

async function identify(browser, name, roomId) {
  // identify
  const page = await browser.newPage();
  await page.goto('http://localhost:8080');
  const input = await page.$('#player-name input[type="text"]');
  await input.type(name);
  const submit = await page.$('form#player-name input[type="submit"]');
  await submit.click();

  // create room
  await page.waitFor('#create-room');
  if (!roomId) {
    const createRoom = await page.$('#create-room');
    await createRoom.click();
  } else { // join room
    const joinRoomInput = await page.$('#join-room input[type="text"]');
    await joinRoomInput.type(roomId);
    const joinRoomSubmit = await page.$('#join-room input[type="submit"]');
    await joinRoomSubmit.click();
  }

  await page.waitFor('#room-id');
  const roomIdInput = await page.$('#room-id');
  roomId = await (await roomIdInput.getProperty('value')).jsonValue();

  return [page, roomId];
}

async function scrapeCurrentWord(page) {
  await page.bringToFront();
  await page.waitFor("#current-word");
  const currentWordEl = await page.$("#current-word");
  const currentWord = await currentWordEl.evaluate(node => node.innerText);
  return currentWord;
}

async function scrapeCurrentWordFromPages(pages) {
  for (let i = 0; i < pages.length; i += 1) {
    let word = await scrapeCurrentWord(pages[i]);
    if (word.length > 0) {
      return word;
    }
  }
}

function delay(ms) {
  return new Promise(resolve => {
    setTimeout(() => {
      resolve();
    }, ms);
  })
}

const STUPID_TAX = 50;

async function submitClue(pages, clue) {
  console.log('submitClue')
  let found = false;
  while (!found) {
    for (let i = 0; i < pages.length; i += 1) {
      let page = pages[i];
      await page.bringToFront();
      await delay(STUPID_TAX);
      const submitClueInput = await page.$('#submit-clue input[type="text"]');
      if (submitClueInput) { // found the clue giver page
        console.log(`found submit clue input on page ${i + 1}`);
        found = true;
        await submitClueInput.type(clue);
        const submitClueSubmit = await page.$('#submit-clue input[type="submit"]')
        if (submitClueSubmit) {
          await submitClueSubmit.click();
          break;
        } else {
          // something weird happened, reset and break
          found = false;
          break;
        }
      } else {
        console.log(`no submit clue input on page ${i + 1}`);
      }
    }
  }
}

async function submitGuess(pages, guess) {
  console.log('submitGuess');
  let found = false;
  while (!found) {
    for (let i = 0; i < pages.length; i += 1) {
      let page = pages[i];
      await page.bringToFront();
      await delay(STUPID_TAX);
      const guessInput = await page.$('#submit-guess input[type="text"]');
      if (guessInput) {
        console.log(`found guess input on page ${i + 1}`);
        found = true;
        await guessInput.type(guess);
        const guessSubmit = await page.$('#submit-guess input[type="submit"]');
        if (guessSubmit) {
          await guessSubmit.click();
          break;
        } else {
          // something weird happened, reset the loop and try again
          found = false;
          break;
        }
      } else {
        console.log(`no guess input on page ${i + 1}`);
      }
    }
  }
}

async function playRound(pages) {
  // trying to do a reset here so the ws go and we update
  for (let i = 0; i < pages.length; i += 1) {
    await pages[i].bringToFront();
  }
  // look for the current word, so we can guess it
  const currentWord = await scrapeCurrentWordFromPages(pages);
  console.log({ currentWord });
  // submit a clue
  await submitClue(pages, 'doesnotmatterwearepsychic');
  console.log('submitted clue');
  // submit guess
  await submitGuess(pages, currentWord);
  console.log('submitted guess');

}

async function scrapeScore(pages) {
  const page = pages[0];
  await page.bringToFront();
  await delay(STUPID_TAX);
  await page.waitFor("#team-a-score");
  const a = await (await page.$("#team-a-score")).evaluate(node => node.innerText);
  const b = await (await page.$("#team-b-score")).evaluate(node => node.innerText);
  return { a, b };
}

async function assertScore(pages, a, b) {
  const score = await scrapeScore(pages);
  if (parseInt(score.a, 10) !== a || parseInt(score.b, 10) !== b) {
    throw new Error(`AssertionError: Expected score to be ${JSON.stringify({ a, b })} but was ${JSON.stringify(score)}`);
  } else {
    console.log(`score: ${JSON.stringify(score)}\n`);
  }
}

async function main() {
  const browser = await puppeteer.launch({ headless: process.env.HEADLESS !== "false" });

  let exception;
  try {
    const [page1, roomId] = await identify(browser, 'one');
    console.log({ roomId });
    const [page2, _1] = await identify(browser, 'two', roomId);
    const [page3, _2] = await identify(browser, 'three', roomId);
    const [page4, _3] = await identify(browser, 'four', roomId);

    // play a round

    // start the game
    await page1.bringToFront();
    await page1.waitFor("#start-game");
    await page1.click("#start-game");

    const pages = [page1, page2, page3, page4];
    console.log('-- round 1');
    await playRound(pages);
    console.log('-- end round 1\n'); // 10 - 0
    await assertScore(pages, 10, 0);

    console.log('-- round 2');
    await playRound(pages);
    console.log('-- end round 2\n'); // 10 - 10 
    await assertScore(pages, 10, 10);

    console.log('-- round 3');
    await playRound(pages);
    console.log('-- end round 3\n'); // 20 - 10 
    await assertScore(pages, 20, 10);

    console.log('-- round 4');
    await playRound(pages);
    console.log('-- end round 4\n'); // 20 - 20 
    await assertScore(pages, 20, 20);

    console.log('-- round 5');
    await playRound(pages);
    console.log('-- end round 5\n'); // 30 - 20 
    await assertScore(pages, 30, 20);

    console.log("OK!")
  } catch (e) {
    exception = e;
    console.error(e);
    await browser.close();
  }

  await browser.close();

  if (exception) {
    throw exception;
  }
}

module.exports = { main };