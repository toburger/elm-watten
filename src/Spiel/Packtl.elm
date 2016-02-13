module Spiel.Packtl (..) where

import Spiel.Kortn exposing (..)


kort forb schlog = Kort forb schlog

schell = kort Schell
herz = kort Herz
oachl = kort Oachl
lab = kort Lab

schellSiebmer = schell Siebmer
schellOchter = schell Ochter
schellNeiner = schell Neiner
schellZehner = schell Zehner
schellUnter = schell Unter
schellOber = schell Ober
schellKinig = schell Kinig
schellAss = schell Ass

herzSiebmer = herz Siebmer
herzOchter = herz Ochter
herzNeiner = herz Neiner
herzZehner = herz Zehner
herzUnter = herz Unter
herzOber = herz Ober
herzKinig = herz Kinig
herzAss = herz Ass

oachlSiebmer = oachl Siebmer
oachlOchter = oachl Ochter
oachlNeiner = oachl Neiner
oachlZehner = oachl Zehner
oachlUnter = oachl Unter
oachlOber = oachl Ober
oachlKinig = oachl Kinig
oachlAss = oachl Ass

labSiebmer = lab Siebmer
labOchter = lab Ochter
labNeiner = lab Neiner
labZehner = lab Zehner
labUnter = lab Unter
labOber = lab Ober
labKinig = lab Kinig
labAss = lab Ass

wheliMit forb = Wheli forb
wheli = Wheli Schell
