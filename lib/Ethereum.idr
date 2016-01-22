module Ethereum

import public Ethereum.GeneralStore
import public Ethereum.Ether
import Effects

%default total
%access public

term syntax EthereumEff "(" {res} ":" [restype] ")" "{" VALUE "=" [v] ";" BALANCE "=" [b] ";" SEND "=" [t] ";" SAVE "=" [s] "}" = DepEff.Eff restype [ETH v b 0 0,STORE] (\res => [ETH v b t s,STORE])
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" VALUE "=" [v] ";" BALANCE "=" [b] ";" SEND "=" [t] ";" SAVE "=" [s] ";" [effs] "}" = DepEff.Eff restype (ETH v b 0 0 :: STORE :: effs) (\res => ETH v b t s :: STORE :: effs)
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" VALUE "=" [v] ";" BALANCE "=" [b] ";" SEND "=" [t] ";" SAVE "=" [s] ";" [ieffs] "=>" [oeffs] "}" = DepEff.Eff restype (ETH v b 0 0 :: STORE :: ieffs) (\res => ETH v b t s :: STORE :: oeffs)

addPlayer : Int -> EthereumEff (success : Bool)
                                              { VALUE   = value
                                              ; BALANCE = balance
                                              ; SEND    = value
                                              ; SAVE    = balance
                                              }
addPlayer = believe_me