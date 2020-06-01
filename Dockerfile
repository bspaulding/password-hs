# Build backend
FROM fpco/stack-build:lts-15.12 as build-hs
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack install --system-ghc

# Build frontend
FROM node:12 as build-elm
WORKDIR /app
RUN npm i create-elm-app
COPY frontend .
RUN ./node_modules/.bin/elm-app build

# Runner
FROM ubuntu:18.04
ENV PORT 80
EXPOSE $PORT
RUN mkdir -p /opt/app
WORKDIR /opt/app
RUN apt-get update && apt-get install -y ca-certificates libgmp-dev
COPY --from=build-hs /root/.local/bin .
COPY words-*.txt ./
COPY --from=build-elm /app/build ./frontend/build
CMD ["/opt/app/password-hs-exe"]
