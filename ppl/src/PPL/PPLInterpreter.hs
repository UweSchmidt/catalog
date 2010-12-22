-- $Id: PPLInterpreter.hs,v 1.10 2001/12/27 15:59:32 uwe Exp $

module PPL.PPLInterpreter where

import PPL.Instructions
import PPL.MachineArchitecture
import PPL.ControlUnit
import PPL.Loader

import PPL.ShowMS

-- -------------------------------------------------------------------

dump = putStr . showMS

tp :: Executable
tp = ( [ Jump	(Disp 1)
       , LoadF  3.14
       , LoadI	23
       , Store	(AbsA 2)
       , Store	(AbsA 0)
       , LoadF	2.5
       , LoadF	3.5
       , Compute	OPmulf
       , Compute	OPf2s
       , SysCall	"writeln"
       , Pop
       , Jump	(Disp 3)
       , Load	(AbsA 0)
       , Store	(AbsA 1)
       , Load	(AbsA 2)
       , Store	(AbsA 3)
       , LoadS	"hello world"
       , SysCall "writeln"
       , Compute OPabort
       , Compute OPterminate
       ]
     , 5
     )

ep = loadExecutable tp

test = execProg ep

