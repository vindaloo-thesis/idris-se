module Ethereum


import public Effects
import public Ethereum.Types
import public Ethereum.Store
import public Ethereum.Ether
import public Ethereum.Environment
import public Ethereum.Commitments

-- %default total
%access public

term syntax EthereumEff "(" {res} ":" [restype] ")" "{" "}" = SimpleEff.Eff restype [ETH v b t s,STORE,ENV sender origin]

-- Syntax extensions for ETH
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" VALUE "=" [v] ";" BALANCE "=" [b] ";" TRANS "=" [t] ";" KEEP "=" [s] "}" = DepEff.Eff restype [ETH v b prev_t prev_s,STORE,ENV contract sender origin] (\res => [ETH v b (t+prev_t) (s+prev_s), STORE, ENV sender origin])
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" VALUE "=" [v] ";" BALANCE "=" [b] ";" TRANS "=" [t] ";" KEEP "=" [s] ";" [effs] "}" = DepEff.Eff restype (ETH v b prev_t prev_s :: STORE :: ENV sender origin :: effs) (\res => ETH v b (t+prev_t) (s+prev_s) :: STORE :: ENV sender origin :: effs)
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" VALUE "=" [v] ";" BALANCE "=" [b] ";" TRANS "=" [t] ";" KEEP "=" [s] ";" [ieffs] "=>" [oeffs] "}" = DepEff.Eff restype (ETH v b prev_t prev_s :: STORE :: ENV sender origin :: ieffs) (\res => ETH v b (t+prev_t) (s+prev_s) :: STORE :: ENV sender origin :: oeffs)
{-
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" VALUE "=" [v] ";" BALANCE "=" [b] ";" TRANS "=" [t] ";" KEEP "=" [s] "}" = DepEff.Eff restype [ETH v b 0 0,STORE,ENV contract sender origin] (\res => [ETH v b (t) (s), STORE, ENV contract sender origin])
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" VALUE "=" [v] ";" BALANCE "=" [b] ";" TRANS "=" [t] ";" KEEP "=" [s] ";" [effs] "}" = DepEff.Eff restype (ETH v b 0 0 :: STORE :: ENV contract sender origin :: effs) (\res => ETH v b (t) (s) :: STORE :: ENV contract sender origin :: effs)
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" VALUE "=" [v] ";" BALANCE "=" [b] ";" TRANS "=" [t] ";" KEEP "=" [s] ";" [ieffs] "=>" [oeffs] "}" = DepEff.Eff restype (ETH v b 0 0 :: STORE :: ENV contract sender origin :: ieffs) (\res => ETH v b (t) (s) :: STORE :: ENV contract sender origin :: oeffs)
-}



-- Syntax extensions for ENV
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" SENDER "=" [s'] ";" ORIGIN "=" [o] "}" = DepEff.Eff restype [ETH v b t s, STORE, ENV s' o] (\res => [ETH v b t s, STORE, ENV s' o])
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" SENDER "=" [s'] ";" ORIGIN "=" [o] ";" [effs] "}" = DepEff.Eff restype (ETH v b t s :: STORE :: ENV s' o :: effs) (\res => ETH v b t s :: STORE :: ENV s' o :: effs)
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" SENDER "=" [s'] ";" ORIGIN "=" [o] ";" [ieffs] "=>" [oeffs] "}" = DepEff.Eff restype (ETH v b t s :: STORE :: ENV s' o :: ieffs) (\res => ETH v b t s :: STORE :: ENV s' o :: oeffs)


-- Syntax extensions for ETH+ENV
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" SENDER "=" [s'] ";" ORIGIN "=" [o] ";" VALUE "=" [v] ";" BALANCE "=" [b] ";" TRANS "=" [t] ";" KEEP "=" [s] "}" = DepEff.Eff restype [ETH v b prev_t prev_s, STORE, ENV s' o] (\res => [ETH v b (t+prev_t) (s+prev_s), STORE, ENV s' o])
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" SENDER "=" [s'] ";" ORIGIN "=" [o] ";" VALUE "=" [v] ";" BALANCE "=" [b] ";" TRANS "=" [t] ";" KEEP "=" [s] ";" [effs] "}" = DepEff.Eff restype (ETH v b prev_t prev_s :: STORE :: ENV s' o :: effs) (\res => ETH v b (t+prev_t) (s+prev_s) :: STORE :: ENV s' o :: effs)
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" SENDER "=" [s'] ";" ORIGIN "=" [o] ";" VALUE "=" [v] ";" BALANCE "=" [b] ";" TRANS "=" [t] ";" KEEP "=" [s] ";" [ieffs] "=>" [oeffs] "}" = DepEff.Eff restype (ETH v b prev_t prev_s :: STORE :: ENV s' o :: ieffs) (\res => ETH v b (t+prev_t) (s+prev_s) :: STORE :: ENV s' o :: oeffs)

{-
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" SENDER "=" [s'] ";" ORIGIN "=" [o] ";" VALUE "=" [v] ";" BALANCE "=" [b] ";" TRANS "=" [t] ";" KEEP "=" [s] "}" = DepEff.Eff restype [ETH v b 0 0, STORE, ENV c s' o] (\res => [ETH v b (t) (s), STORE, ENV c s' o])
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" SENDER "=" [s'] ";" ORIGIN "=" [o] ";" VALUE "=" [v] ";" BALANCE "=" [b] ";" TRANS "=" [t] ";" KEEP "=" [s] ";" [effs] "}" = DepEff.Eff restype (ETH v b 0 0 :: STORE :: ENV c s' o :: effs) (\res => ETH v b (t) (s) :: STORE :: ENV c s' o :: effs)
term syntax EthereumEff "(" {res} ":" [restype] ")" "{" SENDER "=" [s'] ";" ORIGIN "=" [o] ";" VALUE "=" [v] ";" BALANCE "=" [b] ";" TRANS "=" [t] ";" KEEP "=" [s] ";" [ieffs] "=>" [oeffs] "}" = DepEff.Eff restype (ETH v b 0 0  :: STORE :: ENV c s' o :: ieffs) (\res => ETH v b (t) (s) :: STORE :: ENV c s' o :: oeffs)
-}
