FROM ghc
WORKDIR /app/
COPY *.hs ./
RUN ghc Main.hs -o program
RUN chmod +x program
