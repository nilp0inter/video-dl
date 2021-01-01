FROM python:3-alpine
RUN apk add ffmpeg jq
COPY --from=bbvalabsci/kapow:v0.7.0-rc2 /kapow /usr/local/bin/kapow
COPY ./video-dl /video-dl
ADD https://yt-dl.org/downloads/latest/youtube-dl /usr/local/bin/youtube-dl
RUN chmod a+rx /usr/local/bin/youtube-dl
CMD kapow server --debug /video-dl/video-dl
