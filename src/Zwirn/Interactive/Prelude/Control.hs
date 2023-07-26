module Zwirn.Interactive.Prelude.Control where

import qualified Prelude as P ()
import qualified Sound.Tidal.Context as T hiding (fromList)

import Zwirn.Interactive.Meta

-- control pattern stuff

pN :: P (Pattern String -> Pattern Number -> ControlPattern)
pN = toPat $$ toNum (T.tParam T.pF)

pS :: P (Pattern String -> Pattern String -> ControlPattern)
pS = toPat (T.tParam T.pS)

pB :: P (Pattern String -> Pattern Bool -> ControlPattern)
pB = toPat (T.tParam T.pB)

numParams :: [String]
numParams = ["accelerate"
            ,"amp"
            ,"attack"
            ,"bandf"
            ,"bandq"
            ,"begin"
            ,"binshift"
            ,"ccn"
            ,"ccv"
            ,"channel"
            ,"coarse"
            ,"comb"
            ,"cps"
            ,"crush"
            ,"cut"
            ,"cutoff"
            ,"decay"
            ,"delay"
            ,"delaytime"
            ,"detune"
            ,"distort"
            ,"djf"
            ,"dry"
            ,"dur"
            ,"end"
            ,"enhance"
            ,"expression"
            ,"fadeInTime"
            ,"fadeTime"
            ,"freeze"
            ,"freq"
            ,"from"
            ,"fshift"
            ,"gain"
            ,"gate"
            ,"harmonic"
            ,"hbrick"
            ,"hcutoff"
            ,"hold"
            ,"hresonance"
            ,"imag"
            ,"krush"
            ,"lagogo"
            ,"lbrick"
            ,"legato"
            ,"leslie"
            ,"lock"
            ,"midibend"
            ,"miditouch"
            ,"modwheel"
            ,"n"
            ,"note"
            ,"nudge"
            ,"octave"
            ,"octer"
            ,"octersub"
            ,"octersubsub"
            ,"offset"
            ,"orbit"
            ,"overgain"
            ,"overshape"
            ,"pan"
            ,"panorient"
            ,"panspan"
            ,"pansplay"
            ,"panwidth"
            ,"partials"
            ,"phaserdepth"
            ,"phaserrate"
            ,"rate"
            ,"real"
            ,"release"
            ,"resonance"
            ,"ring"
            ,"ringdf"
            ,"ringf"
            ,"room"
            ,"sagogo"
            ,"scram"
            ,"shape"
            ,"size"
            ,"slide"
            ,"smear"
            ,"speed"
            ,"squiz"
            ,"stutterdepth"
            ,"stuttertime"
            ,"sustain"
            ,"sustainpedal"
            ,"timescale"
            ,"timescalewin"
            ,"to"
            ,"toArg"
            ,"tremolodepth"
            ,"tremolorate"
            ,"triode"
            ,"tsdelay"
            ,"velocity"
            ,"voice"
            ,"waveloss"
            ,"xsdelay"
            ,"voi"
            ,"up"
            ,"tremr"
            ,"tremdp"
            ,"sz"
            ,"sus"
            ,"stt"
            ,"std"
            ,"sld"
            ,"scr"
            ,"rel"
            ,"por"
            ,"phasr"
            ,"phasdp"
            ,"number"
            ,"lpq"
            ,"lpf"
            ,"hpq"
            ,"hpf"
            ,"gat"
            ,"fadeOutTime"
            ,"dt"
            ,"dfb"
            ,"det"
            ,"delayt"
            ,"delayfb"
            ,"ctf"
            ,"bpq"
            ,"bpf"
            ,"att"
            ]

stringParams :: [String]
stringParams = ["s","unit","vowel","sound"]


-- functions

(#) :: P (ControlPattern -> ControlPattern -> ControlPattern)
(#) = toPat (T.#)

spin :: P (Pattern Number -> ControlPattern -> ControlPattern)
spin = toPat $$ toNum T.spin

gap :: P (Pattern Number -> ControlPattern -> ControlPattern)
gap = toPat $$ toNum T.gap

slice :: P (Pattern Number -> Pattern Number -> ControlPattern -> ControlPattern)
slice = toPat $$ toNum T.slice

randslice :: P (Pattern Number -> ControlPattern -> ControlPattern)
randslice = toPat $$ toNum T.randslice

splice :: P (Pattern Number -> Pattern Number -> ControlPattern -> ControlPattern)
splice = toPat $$ toNum T.splice

striate :: P (Pattern Number -> ControlPattern -> ControlPattern)
striate = toPat $$ toNum T.striate

striateBy :: P (Pattern Number -> Pattern Number -> ControlPattern -> ControlPattern)
striateBy = toPat $$ toNum T.striateBy

chop :: P (Pattern Number -> ControlPattern -> ControlPattern)
chop = toPat $$ toNum T.chop

loopAt :: P (Pattern Number -> ControlPattern -> ControlPattern)
loopAt = toPat $$ toNum T.loopAt
