# Forced Subtitle Trim
Scan and timestamp forced VOBSUB subtitles for easy media player checking. Trim VOBSUB subtitle tracks to just the forced portions allowing subtitles to be turned on but only showing the forced parts.

Some media players don't handle forced subtitles correctly if they're not a self-contained track leaving you with the option of all the subtitles or nothing at all.

Makes use of MKVToolNix for demux / remux so it needs to be on your path somewhere.
### Command line
```
-describe <paths>           describe all tracks found in mkv files
-scan <paths>               scan VOBSUB tracks timestamping forced subtltles
-trim <track-id> <path>     trim subtitle track to forced portions and remux
```
### Examples
```
fst -describe /movies
fst -scan /movies
fst -trim 4 /movies/film.mkv
```
### Scan output 
```
/movies/film.mkv
	s04 - eng, S_VOBSUB, enabled, default
		00:02:17:815
		00:02:20:891
		00:02:21:320
	s06 - fre, S_VOBSUB, enabled
	s08 - nor, S_VOBSUB, enabled
```
### Reference
```
https://en.wikipedia.org/wiki/MPEG_program_stream
https://en.wikipedia.org/wiki/Packetized_elementary_stream
https://forums.xilinx.com/xlnx/attachments/xlnx/DSPTOOL/15095/1/iso13818-1.pdf
http://dvd.sourceforge.net/dvdinfo/packhdr.html
http://dvd.sourceforge.net/dvdinfo/pes-hdr.html
http://dvd.sourceforge.net/dvdinfo/spu.html
http://sam.zoy.org/writings/dvd/subtitles/
```
sbt assembly to build an executable JAR.
