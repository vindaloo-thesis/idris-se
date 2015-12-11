def idris_Bank_46_Main_46_runDep(LOC0): #Bank.Main.runDep
  loc7 = [2,[0],[4]] #save loc7
  return self.idris_Effects_46_runInit(0, 0, 0, 0, [65678], [1,[65685],LOC0,[0]], loc7)

def idris_Effects_46_runInit(0, 0, 0, 0, loc4{[65678]}, loc5{[1,[65685],LOC0,[0]]}, loc6{[2,[0],[4]]}):
  return self.idris_Effects_46_eff(0, 0, 0, 0, 0, loc5, loc6, [65708, loc4])

def idris_Effects_46_eff(0, 0, 0, 0, 0, loc5{[1,[65685],LOC0,[0]]}, loc6{[2,[0],[4]]}, loc7{[65708, [65678]]}): #Effects.eff
    elif loc6[0] == 2:
      return self.idris_Effects_46_execEff(0, 0, 0, 0, 0, 0, 0, loc5, loc6[1], loc6[2], loc7)

def idris_Effects_46_execEff(0, 0, 0, 0, 0, 0, 0, loc7{[1,[65685],LOC0,[0]]}, loc8{[0]}, loc9{[4]}, loc10{[65708, [65678]]}): #Effects.execEff
    if loc8[0] == 0:
      if loc7[0] == 1:
        loc11 = loc7[1]
        loc12 = loc7[2]
        loc13 = loc7[3]
        loc14 = self.idris_Effects_46_handle(0, 0, 0, 0, 0, 0, loc7[1]){[65681]}
        loc14 = self.idris__123_APPLY0_125_(loc14, loc7[2])({[65680, LOC0]}
        loc14 = self.idris__123_APPLY0_125_(loc14, loc9){[[65679],LOC0,loc1{[4]}]}
        loc15 = [65704,loc10,loc7[1],loc7[2]]
        return self.idris__123_APPLY0_125_(loc14, loc15)

def idris_Effects_46_handle(0, 0, 0, 0, 0, 0, loc6{[65685]}): #Effects.handle
    loc7 = self.idris__123_APPLY0_125_(loc6, 0){[65684]}
    loc7 = self.idris__123_APPLY0_125_(loc7, 0){[65683]}
    loc7 = self.idris__123_APPLY0_125_(loc7, 0){[65682]}
    return self.idris__123_APPLY0_125_(loc7, 0){[65681]}

def idris__123_APPLY0_125_(loc0, loc1{0/LOC0/[4]}): #{APPLY0}
    elif loc0[0] == 65679:
      return self.idris_Bank_46_Main_46__123_runDep2_125_(LOC0, [4], loc1{[65704,[65708,[65678]],[65685],LOC0]})
    elif loc0[0] == 65680:
      loc2 = loc0[1]{LOC0}
      return self.idris_Bank_46_Main_46__123_runDep3_125_(LOC0, loc1{[4]}){[65679],LOC0,loc1{4}}
    elif loc0[0] == 65681:
      return self.idris_Bank_46_Main_46__123_runDep4_125_(loc1)
    elif loc0[0] == 65682:
      return self.idris_Bank_46_Main_46__123_runDep5_125_(loc1)
    elif loc0[0] == 65683: #handle3
      return self.idris_Bank_46_Main_46__123_runDep6_125_(loc1)
    elif loc0[0] == 65684: #handle2
      return self.idris_Bank_46_Main_46__123_runDep7_125_(loc1)
    elif loc0[0] == 65685: #handle1
      return self.idris_Bank_46_Main_46__123_runDep8_125_(loc1)
    elif loc0[0] == 65703: #TODO: CONTINUE FROM HERE /Lego
      loc2 = loc0[1]
      loc3 = loc0[2]
      loc4 = loc0[3]
      loc5 = loc0[4]
      #empty project
      retVal = self.idris_Effects_46__123_execEff0_125_(loc2, loc3, loc4, loc5, loc1)
      return retVal
    elif loc0[0] == 65704:
      return self.idris_Effects_46__123_execEff1_125_([65708,[65678]], [65685], LOC0, [0]){[65703,[65708,[65678]],[0],[65685],LOC0]}
  
def idris_Bank_46_Main_46__123_runDep8_125_(loc0{0}): #Bank.Main.{runDep8}
    return [65684]

def idris_Bank_46_Main_46__123_runDep7_125_(loc0): #Bank.Main.{runDep7}
    return [65683]

def idris_Bank_46_Main_46__123_runDep6_125_(loc0): #Bank.Main.{runDep6}
    return [65682]

def idris_Bank_46_Main_46__123_runDep5_125_(loc0): #Bank.Main.{runDep5}
    return [65681]

def idris_Bank_46_Main_46__123_runDep4_125_(loc0): #Bank.Main.{runDep4}
    retVal = [65680,loc0]

def idris_Bank_46_Main_46__123_runDep3_125_(loc0, loc1): #Bank.Main.{runDep3}
    return [65679,loc0,loc1]

def idris_Bank_46_Main_46__123_runDep2_125_(loc0{LOC0}, loc1{[4]}, loc2{[65704,[65708,[65678]],[65685],LOC0]}): #Bank.Main.{runDep2}
    retVal = self.idris_Effects_46_Ether_46__64_Effects_46_Handler_36_EtherRules_58_SIO_58__33_handle_58_0(0, 0, 0, 0, loc0, loc1, loc2)

def idris_Effects_46_Ether_46__64_Effects_46_Handler_36_EtherRules_58_SIO_58__33_handle_58_0(0, 0, 0, 0, loc4{LOC0}, loc5{[4]}, loc6{[65704,[65708,[65678]],[65685],LOC0]}): #Effects.Ether.@Effects.Handler$EtherRules:SIO:!handle:0
    elif loc5[0] == 4:
      loc7 = self.idris__123_APPLY0_125_(loc6, [0]){[65703,[65708,[65678]],[0],[65685],LOC0]}
      return self.idris__123_APPLY0_125_(loc7, LOC0)

def idris_Effects_46__123_execEff1_125_(loc0{[65708,[65678]]}, loc1{[65685]}, LOC0, loc4{[0]}): #Effects.{execEff1}
    retVal = [65703,loc0,loc3,loc1,loc2]
