# Recap

Recap is short for re-caption.

The original (and main) goal of this tool was to alter captions so that they read better. Since then some additional captions related utilities have been added.

## Background

The need for this tool was born out of editing captions generated from STT tools like [Otter](https://otter.ai/home), [Sonix](https://my.sonix.ai/), [Trint](https://trint.com/) and even [OpenAI Whisper](https://github.com/openai/whisper). These tools do a great job at allowing users to create accurate transcripts quickly, and they also provide the ability to export a captions file from the transcript. However, the captions export only contains some basic options (if any) like: the maximum number of characters per line, number of lines per caption, etc. These captions as is, don't read particularly well. So, some extra work was needed to be done in a caption editor such as [Amara](https://amara.org/).

We realised that much of the editing done in Amara could be accomplished programmatically if we knew the time position of each word. Luckily Otter, Sonix and Trint allow you to export captions with just a one word (or very close to that) per line. With such a caption file we could **restitch** the words and create captions that read more fluently and to our liking. For example, ending a line or caption at a period, semi-colon or long pause just to name a few possibilities.

## Usage

There are several ways to use recap. You only need to choose one of the three options below (listed from easiest, to most involved):

1) Download the stand-alone executable in [Releases](https://github.com/thiru/recap/releases) (if your OS/platform is supported).
2) Install [Babashka](https://github.com/babashka/babashka), clone this repo and run `bin/bb`. This will run recap from source but is just about as fast as the stand-alone binary.
3) Install [Clojure](https://clojure.org/), clone this repo and run `bin/app`. recap launches much slower this way as we're starting up a regular JVM.

### Examples

The examples below assume you're using the stand-alone executable. If you're using Babashka or the Clojure CLI instead simply replace `recap` with `bin/bb` or `bin/app`, respectively.

-----

Restitch the captions in _input.vtt_ to read better and save to _output.vtt_. As mentioned previously this was the original intent of this app. Ideally _input.vtt_ contains captions with as few words per caption as possible. The restitching process works best this way.

```shell
$ recap restitch input.vtt > output.vtt
```

-----

Extend the time each caption appears on screen by 1 second in _input.vtt_ and save to _output.vtt_.

```shell
$ recap linger input.vtt 1 > output.vtt
```

-----

Strip out contiguous speaker tags in _input.vtt_ and save to _output.vtt_. The need for this is due to Trint not having the option to only show a speaker tag when there is a change in speaker. It is either shown in _every single_ caption or not at all.

```shell
$ recap contiguous input.vtt > output.vtt
```

-----

Look for overlapping captions in _input.vtt_ and report them. The need for this came out of a bug in Otter that would create overlapping captions due to an inconsitency between the text and audio during the STT process.

```shell
$ recap overlap input.vtt > output.txt
```

-----

Fixup some common mistakes and follow certain captioning conventions. Many of these are subject.
See [fixup.clj](./src/recap/fixup.clj) the set of regular expressions that apply the changes.

```shell
$ recap fixup input.vtt > output.txt
```

-----

Extract just the text (minus timing metadata, etc.) from _input.vtt_ and save to _output.txt_.

```shell
$ recap text input.vtt > output.txt
```

-----

Convert the captions in _input.vtt_ to essay/paragraph form and save to _output.txt_.

```shell
$ recap essay input.vtt > output.txt
```

-----

Parse _input.vtt_ as a Clojure map and save it to _output.edn_.

```shell
$ recap parse input.vtt > output.edn
```

-----

Download captions from Sonix. For this you need to have a valid Sonix API key specified in _config.edn_.

```shell
$ recap sonix-dl "abc123"
```

Where "abc123" is the id of the respective document in Sonix.

-----

Download captions from Trint. For this you need to have a valid Trint API key specified in _config.edn_.

```shell
$ recap trint-dl srt "abc123"
```

Where "abc123" is the id of the respective document in Trint.

-----

Recap can read from stdin making it easy to pipe commands together like so:

```shell
$ recap sonix-dl "document id" | recap -i restitch | recap -i linger | recap -i fixup
```

-----

Show help

```shell
$ recap --help
```

### Config

See [config.clj](./src/recap/config.clj) for configurable options and their default values.

You can supply your own config by creating an EDN file and placing it in one of the following locations:

- ./config.edn (the current working directory)
- user application data directory
  - $XDG_CONFIG_HOME/recap/config.edn (Linux)
  - %LOCALAPPDATA%/recap/config.edn (Windows)
  - ~/Library/Preferences/recap/config.edn (Mac)
- ~/.config/recap/config.edn

What follows is a description of all available options. All of these apply to the **restitch** command.

- `:absolute-max-chars-per-line`
    - default `47`
    - Specifies a hard upper limit on the number of characters allowed on a line
    - This rule has the highest precedence
- `:breakable-clause-ender-min-chars`
    - default `8`
    - A new line is created if a at least this number of characters have already been seen and the next word ends in a character specified by `:ends-with-clause-ending-punctuation`
- `:breakable-any-punctuation-min-chars`
    - default `23`
    - A new line is created if at least this number of characters have already been seen and the next word ends in a character specified by `:ends-with-any-punctuation`
- `:ends-with-any-punctuation`
    - default `"[,.!?;:\\]'\"—–-]['\"]?$"`
    - Specifies a regular expression that matches text ending in any punctuation mark
    - The only difference from `:ends-with-clause-ending-punctuation` is that this includes a comma
- `:ends-with-clause-ending-punctuation`
    - default `"[.!?;:\\]'\"—–-]['\"]?$"`
    - Specifies a regular expression that matches text ending in a "clause ending" punctuation mark
- `:force-new-cue-tolerance-secs`
    - default `3`
    - A new caption/cue is created if there are at least this many seconds between the current and next word
- `:ideal-max-chars-per-line`
    - default `38`
    - Specifies the ideal number of characters in a line, when not in conflict with any of the other rules
- `:max-lines-per-cue`
    - default `2`
    - Specifies the maximum number of lines per caption/cue
- `:min-cue-duration-secs`
    - default `1.7`
    - Specifies the ideal minimum number of seconds in a cue
    - When a cue is less than this duration an attempt is made to combine it with the previous/next cue

## Development

This app is developed in [Clojure](https://clojure.org/).

To start developing in a REPL:

```shell
$ bin/dev
```

To build as a stand-alone binary using GraalVM:

```shell
$ bin/graalify
```
