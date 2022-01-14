FROM ubuntu:16.04
RUN apt update && apt install ghc -y
WORKDIR /app/
COPY *.hs ./
RUN ghc Main.hs -o program
RUN chmod +x program
