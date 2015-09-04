FROM msaraiva/alpine-erlang-base:3.2

RUN apk --update add erlang && rm -rf /var/cache/apk/*

RUN mkdir -p executor
ADD _build/default/bin executor

CMD ["executor/executor"]
#CMD ["erl", "-noshell"]