FROM ubuntu:20.04
RUN apt update && apt install ghc -y
WORKDIR /app/
COPY *.hs ./
RUN ghc Main.hs -o program
RUN chmod +x program
