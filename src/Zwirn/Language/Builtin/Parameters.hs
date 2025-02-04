{-# LANGUAGE OverloadedStrings #-}

module Zwirn.Language.Builtin.Parameters where

import qualified Data.Map as Map
import Data.Text (Text)
import Zwirn.Core.Map
import Zwirn.Language.Builtin.Internal
import Zwirn.Language.Environment
import Zwirn.Language.Evaluate (Expression, Zwirn, toExp)

builtinParams :: Map.Map Text AnnotatedExpression
builtinParams = addAliases aliases $ Map.unions [builtinTextParams, builtinNumberParams, builtinIntParams]

builtinTextParams :: Map.Map Text AnnotatedExpression
builtinTextParams = Map.unions $ map (\t -> noDesc $ t === toExp ((fmap toExp . singleton (pure t)) :: Zwirn Text -> Zwirn Expression) <:: "Text -> Map") textParams

builtinNumberParams :: Map.Map Text AnnotatedExpression
builtinNumberParams = Map.unions $ map (\t -> noDesc $ t === toExp ((fmap toExp . singleton (pure t)) :: Zwirn Double -> Zwirn Expression) <:: "Number -> Map") numberParams

builtinIntParams :: Map.Map Text AnnotatedExpression
builtinIntParams = Map.unions $ map (\t -> noDesc $ t === toExp ((fmap toExp . singleton (pure t)) :: Zwirn Int -> Zwirn Expression) <:: "Number -> Map") intParams

textParams :: [Text]
textParams = ["s", "unit", "vowel", "toArg"]

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
    "xsdelay"
  ]

aliases :: [(Text, Text)]
aliases =
  [ ("sound", "s"),
    ("voi", "voice"),
    ("up", "n"),
    ("tremr", "tremolorate"),
    ("tremdp", "tremolodepth"),
    ("sz", "size"),
    ("sus", "sustain"),
    ("sld", "slide"),
    ("scr", "scrash"),
    ("rel", "release"),
    ("por", "portamento"),
    ("phasr", "phaserrate"),
    ("phasdp", "phaserdepth"),
    ("number", "n"),
    ("lpq", "resonance"),
    ("lpf", "cutoff"),
    ("hpq", "hresonance"),
    ("hpf", "hcutoff"),
    ("gat", "gate"),
    ("fadeOutTime", "fadeTime"),
    ("dt", "delaytime"),
    ("dfb", "delayfeedback"),
    ("det", "detune"),
    ("delayt", "delaytime"),
    ("delayfb", "delayfeedback"),
    ("ctf", "cutoff"),
    ("bpq", "bandq"),
    ("bpf", "bandf"),
    ("att", "attack")
  ]

addAliases :: [(Text, Text)] -> Map.Map Text AnnotatedExpression -> Map.Map Text AnnotatedExpression
addAliases as x = Map.unions $ map look as ++ [x]
  where
    look (y, n) = case Map.lookup n x of
      Just a -> Map.singleton y a
      Nothing -> Map.empty
