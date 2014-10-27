include ../../../master.Makefile

STOGOUT=$(ROOT)/doc-output

VIDEOS=v1-intro.mp4 \
	demo-intro.mp4 \
	v1-patches.mp4 \
	demo-patches.mp4 \
	v1-ocaml.mp4 \
	demo-ocaml.mp4 \
	v1-markdown.mp4 \
	demo-markdown.mp4 \
	v1-dot.mp4 \
	demo-dot.mp4 \
	v1-mathjax.mp4 \
	demo-mathjax.mp4 \
	v1-math.mp4 \
	demo-math.mp4 \
	v1-end.mp4

all: videos
	$(RM) videos
	for i in $(VIDEOS); do echo "file $$i" >> videos; done
	ffmpeg  -auto_convert 1 -f concat -i videos -c copy -y v1.mp4

.PHONY: videos

videos: $(VIDEOS)

%.png: %.html rasterize.js
	phantomjs rasterize.js file://$(STOGOUT)/videos/1/$< $@ "1200px*700px"

%.mp4: %.png
	ffmpeg -loop 1 -i $< -t 8 -y -pix_fmt yuv420p $@
	#-vf "fade=t=in:st=0.0:d=1.5, fade=t=out:st=5.5:d=1.5"

rasterize.js:
	wget "https://github.com/ariya/phantomjs/raw/master/examples/rasterize.js"

# capture:
# recordmydesktop --width 1200 --height 700 --no-sound --s_quality=10 --fps=25  --delay=2
