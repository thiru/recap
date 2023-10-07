# Recap

Recap is short for re-caption.

The main and original goal of this tool is to alter captions so that they read better.

Some additional utilities around caption files are also provided.

## Background

The need for this tool was born out of using STT tools like [Otter](https://otter.ai/home) and [Trint](https://trint.com/). These tools do a great job at allowing users to create accurate transcripts. They also provide the ability to export a captions file with some simple options like: maximum number of characters per line, number of lines per caption, etc. However these captions as is, don't read particularly well. So, some extra work was needed to be done in a caption editor, such as [Amara](https://amara.org/).

We realised that much of the editing could be accomplished programmatically if we knew the time position of each word. Luckily Otter and Trint allow you to export captions with just a one word (or very close to that) per line. With such a caption file we could **restitch** the words and create captions that read more fluently and to our liking. For example, ending a line or caption at a period, semi-colon or long pause (without audio) just to name a few.

See `default-opts` in [restitch.clj](./src/recap/caption/restitch.clj) for some configurable knobs.

TODO: expose these knobs in CLI.

## Usage

There are several ways to use recap.

1) Likely the easiest way is to download the stand-alone executable in [Releases](https://github.com/thiru/recap/releases) (if your OS/platform is supported) and just run it.
2) The next easiest is to install [Babashka](https://github.com/babashka/babashka), clone this repo and run `bin/bb`. This will run recap from source but is just about as fast as the stand-alone binary.
3) Another option is to install [Clojure](https://clojure.org/), clone this repo and run `bin/app`. recap launches much slower this way as we're starting up a regular JVM.
4) If you're familiar with Clojure you can launch the REPL via `bin/dev` and use recap this way.

### Examples

The examples below assume you're using the stand-alone executable. If you're using Babashka or the Clojure CLI instead simply replace `recap` with `bin/bb` or `bin/app`.

Restitch the captions in _input.vtt_ to read better and save to _output.vtt_:

```shell
$ recap restitch input.vtt > output.vtt
```

Extend the time each caption appears on screen by 1 second in _input.vtt_ and save to _output.vtt_:

```shell
$ recap linger input.vtt 1 > output.vtt
```

Strip out contiguous speaker tags in _input.vtt_ and save to _output.vtt_. The need for this is due to Trint not having the option to only show the speaker tag when there is a change in speaker. It is either always shown in _every single_ caption or not at all:

```shell
$ recap contiguous input.vtt > output.vtt
```

Look for overlapping captions in _input.vtt_ and report them. The need for this came out of a bug in Otter that would create overlapping captions due to an inconsitency between the text and audio during the STT process:

```shell
$ recap text input.vtt > output.txt
```

Extract just the text (minus timing metadata, etc) from _input.vtt_ and save to _output.txt_:

```shell
$ recap text input.vtt > output.txt
```

## Develop

This app is developed in [Clojure](https://clojure.org/).

To start developing in a REPL:

```shell
$ bin/dev
```

To build as a stand-alone binary using GraalVM:

```shell
$ bin/graalify
```

