import "./main.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

const app = Elm.Main.init({
  node: document.getElementById("root")
});

function getSocketHost() {
  const { host, port, protocol } = window.location;
  if (port === "3000") {
    return "ws://localhost:8080";
  } else {
    // host includes port, hostname does not, so no need to append port here
    return `${protocol === "https:" ? "wss" : "ws"}://${host}`;
  }
}

const socket = new WebSocket("ws://localhost:8080");

app.ports.sendMessage.subscribe(function(message) {
  socket.send(message);
});

socket.addEventListener("message", function(event) {
  app.ports.receiveMessage.send(event.data);
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
