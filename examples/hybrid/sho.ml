(* The Zelus compiler, version 2.2-dev
  (2021-06-18-7:42) *)
open Ztypes
open Probzelus
open Infer_importance
open Distribution
type ('d , 'c , 'b , 'a) _sho_sim =
  { mutable major_141 : 'd ;
    mutable i_144 : 'c ; mutable y2_143 : 'b ; mutable y1_142 : 'a }

let sho_sim (cstate_204:Ztypes.cstate) = 
  
  let sho_sim_alloc _ =
    cstate_204.cmax <- (+) cstate_204.cmax  2;
    { major_141 = false ;
      i_144 = (false:bool) ;
      y2_143 = { pos = 42.; der = 0. } ; y1_142 = { pos = 42.; der = 0. } } in
  let sho_sim_copy source dest =
    dest.major_141 <- source.major_141 ;
    dest.i_144 <- source.i_144 ;
    dest.y2_143.pos <- source.y2_143.pos;
    dest.y2_143.der <- source.y2_143.der  ;
    dest.y1_142.pos <- source.y1_142.pos;
    dest.y1_142.der <- source.y1_142.der  in
  let sho_sim_step self ((time_140:float) ,
                         ((theta_137:float) ,
                          (y1_0_138:float) , (y2_0_139:float))) =
    ((let (cindex_205:int) = cstate_204.cindex in
      let cpos_207 = ref (cindex_205:int) in
      cstate_204.cindex <- (+) cstate_204.cindex  2 ;
      self.major_141 <- cstate_204.major ;
      (if cstate_204.major then
       for i_1 = cindex_205 to 1 do Zls.set cstate_204.dvec  i_1  0. done
       else ((self.y2_143.pos <- Zls.get cstate_204.cvec  !cpos_207 ;
              cpos_207 := (+) !cpos_207  1) ;
             (self.y1_142.pos <- Zls.get cstate_204.cvec  !cpos_207 ;
              cpos_207 := (+) !cpos_207  1))) ;
      (let (result_209:(float  * float)) =
           (if self.i_144 then self.y2_143.pos <- y2_0_139) ;
           (if self.i_144 then self.y1_142.pos <- y1_0_138) ;
           self.i_144 <- false ;
           self.y2_143.der <- (-.) ((~-.) self.y1_142.pos) 
                                   (( *. ) theta_137  self.y2_143.pos) ;
           self.y1_142.der <- self.y2_143.pos ;
           (self.y1_142.pos , self.y2_143.pos) in
       cpos_207 := cindex_205 ;
       (if cstate_204.major then
        (((Zls.set cstate_204.cvec  !cpos_207  self.y2_143.pos ;
           cpos_207 := (+) !cpos_207  1) ;
          (Zls.set cstate_204.cvec  !cpos_207  self.y1_142.pos ;
           cpos_207 := (+) !cpos_207  1)))
        else (((Zls.set cstate_204.dvec  !cpos_207  self.y2_143.der ;
                cpos_207 := (+) !cpos_207  1) ;
               (Zls.set cstate_204.dvec  !cpos_207  self.y1_142.der ;
                cpos_207 := (+) !cpos_207  1)))) ; result_209)):float * float) in
   let sho_sim_reset self  =
     (self.i_144 <- true:unit) in
  Cnode { alloc = sho_sim_alloc; copy = sho_sim_copy ;
                                 step = sho_sim_step ; reset = sho_sim_reset }
type ('d , 'c , 'b , 'a) _sho_noisy =
  { mutable major_152 : 'd ;
    mutable i_164 : 'c ; mutable y2_161 : 'b ; mutable y1_160 : 'a }

let sho_noisy (cstate_210:Ztypes.cstate) = 
  
  let sho_noisy_alloc _ =
    cstate_210.cmax <- (+) cstate_210.cmax  2;
    { major_152 = false ;
      i_164 = (false:bool) ;
      y2_161 = { pos = 42.; der = 0. } ; y1_160 = { pos = 42.; der = 0. } } in
  let sho_noisy_copy source dest =
    dest.major_152 <- source.major_152 ;
    dest.i_164 <- source.i_164 ;
    dest.y2_161.pos <- source.y2_161.pos;
    dest.y2_161.der <- source.y2_161.der  ;
    dest.y1_160.pos <- source.y1_160.pos;
    dest.y1_160.der <- source.y1_160.der  in
  let sho_noisy_step self ((time_151:float) ,
                           ((prob_145:Infer_importance.prob) ,
                            ((z_150:zero) ,
                             (theta_147:float) ,
                             (sigma_146:float) ,
                             (y1_0_148:float) , (y2_0_149:float)))) =
    ((let (cindex_211:int) = cstate_210.cindex in
      let cpos_213 = ref (cindex_211:int) in
      cstate_210.cindex <- (+) cstate_210.cindex  2 ;
      self.major_152 <- cstate_210.major ;
      (if cstate_210.major then
       for i_1 = cindex_211 to 1 do Zls.set cstate_210.dvec  i_1  0. done
       else ((self.y2_161.pos <- Zls.get cstate_210.cvec  !cpos_213 ;
              cpos_213 := (+) !cpos_213  1) ;
             (self.y1_160.pos <- Zls.get cstate_210.cvec  !cpos_213 ;
              cpos_213 := (+) !cpos_213  1))) ;
      (let (result_215:((float  * float))signal) =
           let obsp_163 = ref (false:bool) in
           let obsv_162 = ref ((42. , 42.):float * float) in
           (if self.i_164 then self.y2_161.pos <- y2_0_149) ;
           (if self.i_164 then self.y1_160.pos <- y1_0_148) ;
           self.i_164 <- false ;
           (begin match z_150 with
                  | true ->
                      obsp_163 := true ;
                      obsv_162 := ((Infer_importance.sample' (prob_145 ,
                                                              (Distribution.gaussian 
                                                                 (self.y1_160.pos
                                                                  , sigma_146))))
                                   ,
                                   (Infer_importance.sample' (prob_145 ,
                                                              (Distribution.gaussian 
                                                                 (self.y2_161.pos
                                                                  , sigma_146)))))
                  | _ -> ()  end) ;
           self.y2_161.der <- (-.) ((~-.) self.y1_160.pos) 
                                   (( *. ) theta_147  self.y2_161.pos) ;
           self.y1_160.der <- self.y2_161.pos ; (!obsv_162 , !obsp_163) in
       cpos_213 := cindex_211 ;
       (if cstate_210.major then
        (((Zls.set cstate_210.cvec  !cpos_213  self.y2_161.pos ;
           cpos_213 := (+) !cpos_213  1) ;
          (Zls.set cstate_210.cvec  !cpos_213  self.y1_160.pos ;
           cpos_213 := (+) !cpos_213  1)))
        else (((Zls.set cstate_210.dvec  !cpos_213  self.y2_161.der ;
                cpos_213 := (+) !cpos_213  1) ;
               (Zls.set cstate_210.dvec  !cpos_213  self.y1_160.der ;
                cpos_213 := (+) !cpos_213  1)))) ; result_215)):(float *
                                                                 float)
                                                                signal) in 
  let sho_noisy_reset self  =
    (self.i_164 <- true:unit) in
  Cnode { alloc = sho_noisy_alloc; copy = sho_noisy_copy ;
                                   step = sho_noisy_step ;
                                   reset = sho_noisy_reset }
type ('e , 'd , 'c , 'b , 'a) _sho_model =
  { mutable i_200 : 'e ;
    mutable major_172 : 'd ;
    mutable i_177 : 'c ; mutable theta_174 : 'b ; mutable sigma_173 : 'a }

let sho_model (cstate_216:Ztypes.cstate) = 
  let Cnode { alloc = i_200_alloc; copy = i_200_copy ;
                                   step = i_200_step ; reset = i_200_reset } = sho_sim 
  cstate_216 in
  let sho_model_alloc _ =
    ();
    { major_172 = false ;
      i_177 = (false:bool) ;
      theta_174 = (42.:float) ; sigma_173 = (42.:float);
      i_200 = i_200_alloc () (* continuous *)  } in
  let sho_model_copy source dest =
    dest.major_172 <- source.major_172 ;
    dest.i_177 <- source.i_177 ;
    dest.theta_174 <- source.theta_174 ; dest.sigma_173 <- source.sigma_173;
    i_200_copy source.i_200 dest.i_200 (* continuous *) in
  let sho_model_step self ((time_171:float) ,
                           ((prob_165:Infer_importance.prob) ,
                            ((((obsv_168:float) , (obsv_169:float)) ,
                              (obsp_170:bool)) ,
                             (y1_0_166:float) , (y2_0_167:float)))) =
    ((self.major_172 <- cstate_216.major ;
      ((if self.i_177 then
        self.sigma_173 <- Infer_importance.sample' (prob_165 ,
                                                    (Distribution.gaussian 
                                                       (0.5 , 0.1)))) ;
       (if self.i_177 then
        self.theta_174 <- Infer_importance.sample' (prob_165 ,
                                                    (Distribution.gaussian 
                                                       (0. , 0.5)))) ;
       self.i_177 <- false ;
       (let ((y1_175:float) , (y2_176:float)) =
            i_200_step self.i_200
              (time_171 , (self.theta_174 , y1_0_166 , y2_0_167)) in
        (begin match ((obsv_168 , obsv_169) , obsp_170) with
               | (((y1_obs_178:float) , (y2_obs_179:float)) , true) ->
                   let () =
                       Infer_importance.observe' (prob_165 ,
                                                  ((Distribution.gaussian 
                                                      (y2_176 ,
                                                       (( ** ) self.sigma_173
                                                                2.))) ,
                                                   y2_obs_179)) in
                   let () =
                       Infer_importance.observe' (prob_165 ,
                                                  ((Distribution.gaussian 
                                                      (y1_175 ,
                                                       (( ** ) self.sigma_173
                                                                2.))) ,
                                                   y1_obs_178)) in
                   () | _ -> ()  end) ; (self.theta_174 , self.sigma_173)))):
    float * float) in 
  let sho_model_reset self  =
    ((self.i_177 <- true ; i_200_reset self.i_200 ):unit) in
  Cnode { alloc = sho_model_alloc; copy = sho_model_copy ;
                                   step = sho_model_step ;
                                   reset = sho_model_reset }
type ('e , 'd , 'c , 'b , 'a) _horizon =
  { mutable major_182 : 'e ;
    mutable i_186 : 'd ;
    mutable x_185 : 'c ; mutable x_184 : 'b ; mutable t_183 : 'a }

let horizon (cstate_222:Ztypes.cstate) = 
  
  let horizon_alloc _ =
    cstate_222.cmax <- (+) cstate_222.cmax  1 ;
    cstate_222.zmax <- (+) cstate_222.zmax  2;
    { major_182 = false ;
      i_186 = (false:bool) ;
      x_185 = { zin = false; zout = 1. } ;
      x_184 = { zin = false; zout = 1. } ; t_183 = { pos = 42.; der = 0. } } in
  let horizon_copy source dest =
    dest.major_182 <- source.major_182 ;
    dest.i_186 <- source.i_186 ;
    dest.x_185.zin <- source.x_185.zin;dest.x_185.zout <- source.x_185.zout 
    ;
    dest.x_184.zin <- source.x_184.zin;dest.x_184.zout <- source.x_184.zout 
    ; dest.t_183.pos <- source.t_183.pos;dest.t_183.der <- source.t_183.der  in
  let horizon_step self ((time_181:float) , (h_180:float)) =
    ((let (cindex_223:int) = cstate_222.cindex in
      let cpos_225 = ref (cindex_223:int) in
      let (zindex_224:int) = cstate_222.zindex in
      let zpos_226 = ref (zindex_224:int) in
      cstate_222.cindex <- (+) cstate_222.cindex  1 ;
      cstate_222.zindex <- (+) cstate_222.zindex  2 ;
      self.major_182 <- cstate_222.major ;
      (if cstate_222.major then
       for i_1 = cindex_223 to 0 do Zls.set cstate_222.dvec  i_1  0. done
       else ((self.t_183.pos <- Zls.get cstate_222.cvec  !cpos_225 ;
              cpos_225 := (+) !cpos_225  1))) ;
      (let (result_227:zero) =
           (if self.i_186 then self.t_183.pos <- (~-.) h_180) ;
           self.i_186 <- false ;
           (begin match self.x_184.zin with
                  | true -> self.t_183.pos <- (~-.) h_180 | _ -> ()  end) ;
           self.x_184.zout <- self.t_183.pos ;
           self.t_183.der <- 1. ;
           self.x_185.zout <- self.t_183.pos ; self.x_185.zin in
       cpos_225 := cindex_223 ;
       (if cstate_222.major then
        (((Zls.set cstate_222.cvec  !cpos_225  self.t_183.pos ;
           cpos_225 := (+) !cpos_225  1)) ;
         ((self.x_185.zin <- false) ; (self.x_184.zin <- false)))
        else (((self.x_185.zin <- Zls.get_zin cstate_222.zinvec  !zpos_226 ;
                zpos_226 := (+) !zpos_226  1) ;
               (self.x_184.zin <- Zls.get_zin cstate_222.zinvec  !zpos_226 ;
                zpos_226 := (+) !zpos_226  1)) ;
              zpos_226 := zindex_224 ;
              ((Zls.set cstate_222.zoutvec  !zpos_226  self.x_185.zout ;
                zpos_226 := (+) !zpos_226  1) ;
               (Zls.set cstate_222.zoutvec  !zpos_226  self.x_184.zout ;
                zpos_226 := (+) !zpos_226  1)) ;
              ((Zls.set cstate_222.dvec  !cpos_225  self.t_183.der ;
                cpos_225 := (+) !cpos_225  1)))) ; result_227)):zero) in 
  let horizon_reset self  =
    (self.i_186 <- true:unit) in
  Cnode { alloc = horizon_alloc; copy = horizon_copy ;
                                 step = horizon_step ; reset = horizon_reset }
type ('d , 'c , 'b , 'a) _main =
  { mutable i_203 : 'd ;
    mutable i_202 : 'c ; mutable i_201 : 'b ; mutable major_188 : 'a }

let main (cstate_228:Ztypes.cstate) = 
  let Cnode { alloc = i_203_alloc; copy = i_203_copy ;
                                   step = i_203_step ; reset = i_203_reset } = 
  Infer_importance.hybrid_gen sho_noisy cstate_228 in 
  let Cnode { alloc = i_202_alloc; copy = i_202_copy ;
                                   step = i_202_step ; reset = i_202_reset } = horizon 
  cstate_228 in 
  let Cnode { alloc = i_201_alloc; copy = i_201_copy ;
                                   step = i_201_step ; reset = i_201_reset } = 
  Infer_importance.hybrid_infer 10000  sho_model cstate_228 in
  let main_alloc _ =
    ();
    { major_188 = false;
      i_203 = i_203_alloc () (* continuous *)  ;
      i_202 = i_202_alloc () (* continuous *)  ;
      i_201 = i_201_alloc () (* continuous *)  } in
  let main_copy source dest =
    dest.major_188 <- source.major_188;
    i_203_copy source.i_203 dest.i_203 (* continuous *) ;
    i_202_copy source.i_202 dest.i_202 (* continuous *) ;
    i_201_copy source.i_201 dest.i_201 (* continuous *) in
  let main_step self ((time_187:float) , ()) =
    ((self.major_188 <- cstate_228.major ;
      (let ((((obsv_196:float) , (obsv_197:float)) , (copy_199:bool)) , _) =
           i_203_step self.i_203
             (time_187 ,
              ((i_202_step self.i_202 (time_187 , 0.1)) ,
               (-0.15) , 0.1 , 1. , 0.)) in
       let (dist_191:((float  * float))Distribution.t) =
           i_201_step self.i_201
             (time_187 , (((obsv_196 , obsv_197) , copy_199) , 1. , 0.)) in
       let ((d_theta_190:(float)Distribution.t) ,
            (d_sigma_189:(float)Distribution.t)) =
           Distribution.split dist_191 in
       (begin match ((obsv_196 , obsv_197) , copy_199) with
              | (_ , true) ->
                  let _ = print_string "theta mean: " in
                  let _ = print_float (Distribution.mean_float d_theta_190) in
                  let _ = print_string " sigma_mean: " in
                  let _ = print_float (Distribution.mean_float d_sigma_189) in
                  let () = print_newline () in
                  () | _ -> ()  end) ; ())):unit) in 
  let main_reset self  =
    ((i_203_reset self.i_203  ;
      i_202_reset self.i_202  ; i_201_reset self.i_201 ):unit) in
  Cnode { alloc = main_alloc; copy = main_copy ;
                              step = main_step ; reset = main_reset }
