{-# LANGUAGE OverloadedStrings #-}

module Zwirn.Language.Builtin.Parameters where

import qualified Data.Map as Map
import Data.Text (Text)
import Zwirn.Language.Builtin.Internal
import Zwirn.Language.Environment
import Zwirn.Language.Evaluate (Expression, Zwirn, toExp)
import Zwirn.Language.Evaluate.Internal

builtinParams :: [Map.Map Text AnnotatedExpression]
builtinParams = [builtinTextParams, builtinNumberParams, builtinIntParams]

builtinTextParams :: Map.Map Text AnnotatedExpression
builtinTextParams = Map.unions $ map (\t -> noDesc $ t === toExp (singMap $ pure t :: Zwirn Text -> Zwirn Expression) <:: "Text -> Map") textParams

builtinNumberParams :: Map.Map Text AnnotatedExpression
builtinNumberParams = Map.unions $ map (\t -> noDesc $ t === toExp (singMap $ pure t :: Zwirn Double -> Zwirn Expression) <:: "Number -> Map") numberParams

builtinIntParams :: Map.Map Text AnnotatedExpression
builtinIntParams = Map.unions $ map (\t -> noDesc $ t === toExp (singMap $ pure t :: Zwirn Int -> Zwirn Expression) <:: "Number -> Map") intParams

textParams :: [Text]
textParams = ["s", "unit", "vowel", "sound", "toArg"]

intParams :: [Text]
intParams = ["cut", "orbit"]

numberParams :: [Text]
numberParams =
  [ "accelerate",
    "amp",
    "attack",
    "bandf",
    "bandq",
    "begin",
    "binshift",
    "ccn",
    "ccv",
    "channel",
    "coarse",
    "comb",
    "crush",
    "cutoff",
    "decay",
    "delay",
    "delaytime",
    "detune",
    "distort",
    "djf",
    "dry",
    "dur",
    "end",
    "enhance",
    "expression",
    "fadeInTime",
    "fadeTime",
    "freeze",
    "freq",
    "from",
    "fshift",
    "gain",
    "gate",
    "harmonic",
    "hbrick",
    "hcutoff",
    "hold",
    "hresonance",
    "imag",
    "krush",
    "lagogo",
    "lbrick",
    "legato",
    "leslie",
    "lock",
    "midibend",
    "miditouch",
    "modwheel",
    "n",
    "note",
    "nudge",
    "octave",
    "octer",
    "octersub",
    "octersubsub",
    "offset",
    "overgain",
    "overshape",
    "pan",
    "panorient",
    "panspan",
    "pansplay",
    "panwidth",
    "partials",
    "phaserdepth",
    "phaserrate",
    "rate",
    "real",
    "release",
    "resonance",
    "ring",
    "ringdf",
    "ringf",
    "room",
    "sagogo",
    "scram",
    "shape",
    "size",
    "slide",
    "smear",
    "speed",
    "squiz",
    "stutterdepth",
    "stuttertime",
    "sustain",
    "sustainpedal",
    "timescale",
    "timescalewin",
    "to",
    "tremolodepth",
    "tremolorate",
    "triode",
    "tsdelay",
    "velocity",
    "voice",
    "waveloss",
    "xsdelay",
    "voi",
    "up",
    "tremr",
    "tremdp",
    "sz",
    "sus",
    "stt",
    "std",
    "sld",
    "scr",
    "rel",
    "por",
    "phasr",
    "phasdp",
    "number",
    "lpq",
    "lpf",
    "hpq",
    "hpf",
    "gat",
    "fadeOutTime",
    "dt",
    "dfb",
    "det",
    "delayt",
    "delayfb",
    "ctf",
    "bpq",
    "bpf",
    "att"
  ]
