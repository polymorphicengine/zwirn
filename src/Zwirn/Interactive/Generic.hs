{-# LANGUAGE TemplateHaskell #-}
module Zwirn.Interactive.Generic where

{-
    Generic.hs - contains a list of all parameter functions and
    provides TH functions to generate their declarations
    Copyright (C) 2023, Martin Gius

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}

import Language.Haskell.TH
import Zwirn.Interactive.Types (P, NumberPattern, TextPattern, ControlPattern)
import Zwirn.Interactive.Transform (_toPat)
import Zwirn.Interactive.Convert (_toTarget)
import Zwirn.Interactive.TidalT (_chordMaker)
import qualified Prelude as P

_mkStringParams :: [P.String] -> Q [Dec]
_mkStringParams names = P.return (P.map oneDec names P.++ P.map sig names)
                  where oneDec name = ValD (VarP (mkName name)) (NormalB (AppE (VarE '_toPat) (AppE (VarE '_toTarget) (VarE (mkName ("T." P.++ name)))))) []
                        sig name = SigD (mkName name) (AppT (ConT ''P) (AppT (AppT ArrowT (ConT ''TextPattern)) (ConT ''ControlPattern)))

_mkNumParams :: [P.String] -> Q [Dec]
_mkNumParams names = P.return (P.map oneDec names P.++ P.map sig names)
                  where oneDec name = ValD (VarP (mkName name)) (NormalB (AppE (VarE '_toPat) (AppE (VarE '_toTarget) (VarE (mkName ("T." P.++ name)))))) []
                        sig name = SigD (mkName name) (AppT (ConT ''P) (AppT (AppT ArrowT (ConT ''NumberPattern)) (ConT ''ControlPattern)))

_mkChords :: [P.String] -> Q [Dec]
_mkChords names = P.return (P.map oneDec names P.++ P.map sig names)
                  where oneDec name = ValD (VarP (mkName name)) (NormalB (AppE (VarE '_chordMaker) (LitE (StringL name)))) []
                        sig name = SigD (mkName name) (AppT (ConT ''P) (AppT (AppT ArrowT (ConT ''NumberPattern)) (ConT ''NumberPattern)))

_mkNotes :: [P.String] -> Q [Dec]
_mkNotes names = P.return (P.map oneDec names P.++ P.map sig names)
                  where oneDec name = ValD (VarP (mkName name)) (NormalB (AppE (VarE '_noteVal) (LitE (StringL name)))) []
                        sig name = SigD (mkName name) (AppT (ConT ''P) (ConT ''NumberPattern))

_stringParams :: [P.String]
_stringParams = ["s","unit","vowel","sound","toArg"]

_numParams :: [P.String]
_numParams = ["accelerate"
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

_chords :: [P.String]
_chords = ["major","maj","aug","plus","sharp5","six","sixNine","six9","sixby9","major7","maj7","major9","maj9","add9","major11","maj11","add11","major13","maj13","add13","dom7"
          ,"dom9","dom11","dom13","sevenFlat5","sevenSharp5","sevenFlat9","nine","eleven","thirteen","minor","min","m","diminished","dim","minorSharp5","msharp5"
          ,"mS5","minor6","min6","m6","minorSixNine","minor69","min69","minSixNine","m69","mSixNine","m6by9","minor7flat5","minor7f5","min7flat5","min7f5","m7flat5","m7f5","minor7","min7"
          ,"m7","minor7sharp5","minor7s5","min7sharp5","min7s5","m7sharp5","m7s5","minor7flat9","minor7f9","min7flat9","min7f9","m7flat9","m7f9","minor7sharp9","minor7s9","min7sharp9","min7s9"
          ,"m7sharp9","m7s9","diminished7","dim7","minor9","min9","m9","minor11","min11","m11","minor13","min13","m13","one","five","sus2","sus4","sevenSus2","sevenSus4"
          ,"nineSus4","ninesus4","sevenFlat10","nineSharp5","minor9sharp5","minor9s5","min9sharp5","min9s5","m9sharp5","m9s5","sevenSharp5flat9","minor7sharp5flat9"
          ,"m7sharp5flat9","elevenSharp","minor11sharp","m11sharp","m11s"]

_noteNames :: [P.String]
_noteNames = ["c", "d", "e", "f", "g", "a", "h", "b"]

_noteMods :: [P.String]
_noteMods = ["", "f", "s"]

_noteOctaves :: [P.String]
_noteOctaves = ["", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

_notes :: [P.String]
_notes = [n P.++ m P.++ o | n <- _noteNames, m <- _noteMods, o <- _noteOctaves]

_nameVal :: P.Char -> P.Int
_nameVal 'c' = 0
_nameVal 'd' = 2
_nameVal 'e' = 4
_nameVal 'f' = 5
_nameVal 'g' = 7
_nameVal 'a' = 9
_nameVal 'h' = 11
_nameVal _ = 0

_octVal :: P.Char -> P.Int
_octVal '0' = -60
_octVal '1' = -48
_octVal '2' = -36
_octVal '3' = -24
_octVal '4' = -12
_octVal '5' = 0
_octVal '6' = 12
_octVal '7' = 24
_octVal '8' = 36
_octVal '9' = 48
_octVal _ = 0

_noteVal :: P.String -> NumberPattern
_noteVal (c:[]) = P.pure P.$ _toTarget (_nameVal c)
_noteVal (c:'f':[]) =  P.pure P.$ _toTarget (_nameVal c P.- 1)
_noteVal (c:'s':[]) =  P.pure P.$ _toTarget (_nameVal c P.+ 1)
_noteVal (c:'f':x:_) =  P.pure P.$ _toTarget (_nameVal c P.- 1 P.+ _octVal x)
_noteVal (c:'s':x:_) =  P.pure P.$ _toTarget (_nameVal c P.+ 1 P.+ _octVal x)
_noteVal (c:x:_) =  P.pure P.$ _toTarget (_nameVal c  P.+ _octVal x)
_noteVal _ = 0
