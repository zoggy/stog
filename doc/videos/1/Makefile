include ../../../master.Makefile

STOGOUT=$(ROOT)/doc-output

VIDEOS=v1-intro.mp4 \
	v1-patches.mp4 \
	v1-ocaml.mp4 \
	v1-markdown.mp4

all: videos
	$(RM) videos
	for i in $(VIDEOS); do echo "file $$i" >> videos; done
	ffmpeg -f concat -i videos -y v1.mp4

.PHONY: videos

videos: $(VIDEOS)

%.png: %.html rasterize.js
	phantomjs rasterize.js file://$(STOGOUT)/videos/1/$< $@ "1024px*700px"

%.mp4: %.png
	ffmpeg -loop 1 -i $< -t 8 -vf "fade=t=in:st=0.0:d=1.5, fade=t=out:st=5.5:d=1.5" \
	-y -pix_fmt yuv420p $@

rasterize.js:
	wget "https://github.com/ariya/phantomjs/raw/master/examples/rasterize.js"
