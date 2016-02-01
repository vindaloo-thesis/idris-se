module Ethereum


import public Effects
import public Ethereum.Types
import public Ethereum.Store
import public Ethereum.Ether
import public Ethereum.Environment

-- %default total
%access public

-- Syntax extensions for ETH
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" VALUE "=" [v] ";" BALANCE "=" [b] ";" SEND "=" [t] ";" SAVE "=" [s] "}" = DepEff.Eff restype [ETH v b prev_t prev_s,STORE] (\res => [ETH v b (prev_t+t) (prev_s+s), STORE, ENV contract sender origin])
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" VALUE "=" [v] ";" BALANCE "=" [b] ";" SEND "=" [t] ";" SAVE "=" [s] ";" [effs] "}" = DepEff.Eff restype (ETH v b prev_t prev_s :: STORE :: effs) (\res => ETH v b (prev_t+t) (prev_s+s) :: STORE :: ENV contract sender origin :: effs)
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" VALUE "=" [v] ";" BALANCE "=" [b] ";" SEND "=" [t] ";" SAVE "=" [s] ";" [ieffs] "=>" [oeffs] "}" = DepEff.Eff restype (ETH v b prev_t prev_s :: STORE :: ieffs) (\res => ETH v b (prev_t+t) (prev_s+s) :: STORE :: ENV contract sender origin :: oeffs)


-- Syntax extensions for ENV
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" ADDRESS "=" [c] ";" SENDER "=" [s'] ";" ORIGIN "=" [o] "}" = DepEff.Eff restype [ETH v b t s, STORE, ENV c s' o] (\res => [ETH v b t s, STORE, ENV c s' o])
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" ADDRESS "=" [c] ";" SENDER "=" [s'] ";" ORIGIN "=" [o] ";" [effs] "}" = DepEff.Eff restype (ETH v b t s :: STORE :: ENV c s' o :: effs) (\res => ETH v b t s :: STORE :: ENV c s' o :: effs)
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" ADDRESS "=" [c] ";" SENDER "=" [s'] ";" ORIGIN "=" [o] ";" [ieffs] "=>" [oeffs] "}" = DepEff.Eff restype (ETH v b t s :: STORE :: ENV c s' o :: ieffs) (\res => ETH v b t s :: STORE :: ENV c s' o :: oeffs)


-- Syntax extensions for ETH+ENV
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" ADDRESS "=" [c] ";" SENDER "=" [s'] ";" ORIGIN "=" [o] ";" VALUE "=" [v] ";" BALANCE "=" [b] ";" SEND "=" [t] ";" SAVE "=" [s] "}" = DepEff.Eff restype [ETH v b prev_t prev_s, STORE, ENV c s' o] (\res => [ETH v b (prev_t+t) (prev_s+s), STORE, ENV c s' o])
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" ADDRESS "=" [c] ";" SENDER "=" [s'] ";" ORIGIN "=" [o] ";" VALUE "=" [v] ";" BALANCE "=" [b] ";" SEND "=" [t] ";" SAVE "=" [s] ";" [effs] "}" = DepEff.Eff restype (ETH v b prev_t prev_s :: STORE :: ENV c s' o :: effs) (\res => ETH v b (prev_t+t) (prev_s+s) :: STORE :: ENV c s' o :: effs)
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" ADDRESS "=" [c] ";" SENDER "=" [s'] ";" ORIGIN "=" [o] ";" VALUE "=" [v] ";" BALANCE "=" [b] ";" SEND "=" [t] ";" SAVE "=" [s] ";" [ieffs] "=>" [oeffs] "}" = DepEff.Eff restype (ETH v b prev_t prev_s :: STORE :: ENV c s' o :: ieffs) (\res => ETH v b (prev_t+t) (prev_s+s) :: STORE :: ENV c s' o :: oeffs)
