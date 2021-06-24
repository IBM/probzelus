(* The Zelus compiler, version 2.2-dev
  (2021-06-18-7:42) *)
open Ztypes
open Probzelus
open Infer_importance
open Distribution
type ('d , 'c , 'b , 'a) _sho_sim =
  { mutable major_138 : 'd ;
    mutable i_141 : 'c ; mutable y2_140 : 'b ; mutable y1_139 : 'a }

let sho_sim (cstate_200:Ztypes.cstate) = 
  
  let sho_sim_alloc _ =
    cstate_200.cmax <- (+) cstate_200.cmax  2;
    { major_138 = false ;
      i_141 = (false:bool) ;
      y2_140 = { pos = 42.; der = 0. } ; y1_139 = { pos = 42.; der = 0. } } in
  let sho_sim_copy source dest =
    dest.major_138 <- source.major_138 ;
    dest.i_141 <- source.i_141 ;
    dest.y2_140.pos <- source.y2_140.pos;
    dest.y2_140.der <- source.y2_140.der  ;
    dest.y1_139.pos <- source.y1_139.pos;
    dest.y1_139.der <- source.y1_139.der  in
  let sho_sim_step self ((time_137:float) ,
                         ((theta_134:float) ,
                          (y1_0_135:float) , (y2_0_136:float))) =
    ((let (cindex_201:int) = cstate_200.cindex in
      let cpos_203 = ref (cindex_201:int) in
      cstate_200.cindex <- (+) cstate_200.cindex  2 ;
      self.major_138 <- cstate_200.major ;
      (if cstate_200.major then
       for i_1 = cindex_201 to 1 do Zls.set cstate_200.dvec  i_1  0. done
       else ((self.y2_140.pos <- Zls.get cstate_200.cvec  !cpos_203 ;
              cpos_203 := (+) !cpos_203  1) ;
             (self.y1_139.pos <- Zls.get cstate_200.cvec  !cpos_203 ;
              cpos_203 := (+) !cpos_203  1))) ;
      (let (result_205:(float  * float)) =
           (if self.i_141 then self.y2_140.pos <- y2_0_136) ;
           (if self.i_141 then self.y1_139.pos <- y1_0_135) ;
           self.i_141 <- false ;
           self.y2_140.der <- (-.) ((~-.) self.y1_139.pos) 
                                   (( *. ) theta_134  self.y2_140.pos) ;
           self.y1_139.der <- self.y2_140.pos ;
           (self.y1_139.pos , self.y2_140.pos) in
       cpos_203 := cindex_201 ;
       (if cstate_200.major then
        (((Zls.set cstate_200.cvec  !cpos_203  self.y2_140.pos ;
           cpos_203 := (+) !cpos_203  1) ;
          (Zls.set cstate_200.cvec  !cpos_203  self.y1_139.pos ;
           cpos_203 := (+) !cpos_203  1)))
        else (((Zls.set cstate_200.dvec  !cpos_203  self.y2_140.der ;
                cpos_203 := (+) !cpos_203  1) ;
               (Zls.set cstate_200.dvec  !cpos_203  self.y1_139.der ;
                cpos_203 := (+) !cpos_203  1)))) ; result_205)):float * float) in
   let sho_sim_reset self  =
     (self.i_141 <- true:unit) in
  Cnode { alloc = sho_sim_alloc; copy = sho_sim_copy ;
                                 step = sho_sim_step ; reset = sho_sim_reset }
type ('d , 'c , 'b , 'a) _sho_noisy =
  { mutable major_148 : 'd ;
    mutable i_160 : 'c ; mutable y2_157 : 'b ; mutable y1_156 : 'a }

let sho_noisy (cstate_206:Ztypes.cstate) = 
  
  let sho_noisy_alloc _ =
    cstate_206.cmax <- (+) cstate_206.cmax  2;
    { major_148 = false ;
      i_160 = (false:bool) ;
      y2_157 = { pos = 42.; der = 0. } ; y1_156 = { pos = 42.; der = 0. } } in
  let sho_noisy_copy source dest =
    dest.major_148 <- source.major_148 ;
    dest.i_160 <- source.i_160 ;
    dest.y2_157.pos <- source.y2_157.pos;
    dest.y2_157.der <- source.y2_157.der  ;
    dest.y1_156.pos <- source.y1_156.pos;
    dest.y1_156.der <- source.y1_156.der  in
  let sho_noisy_step self ((time_147:float) ,
                           ((z_146:zero) ,
                            (theta_143:float) ,
                            (sigma_142:float) ,
                            (y1_0_144:float) , (y2_0_145:float))) =
    ((let (cindex_207:int) = cstate_206.cindex in
      let cpos_209 = ref (cindex_207:int) in
      cstate_206.cindex <- (+) cstate_206.cindex  2 ;
      self.major_148 <- cstate_206.major ;
      (if cstate_206.major then
       for i_1 = cindex_207 to 1 do Zls.set cstate_206.dvec  i_1  0. done
       else ((self.y2_157.pos <- Zls.get cstate_206.cvec  !cpos_209 ;
              cpos_209 := (+) !cpos_209  1) ;
             (self.y1_156.pos <- Zls.get cstate_206.cvec  !cpos_209 ;
              cpos_209 := (+) !cpos_209  1))) ;
      (let (result_211:((float  * float))signal) =
           let obsp_159 = ref (false:bool) in
           let obsv_158 = ref ((42. , 42.):float * float) in
           (if self.i_160 then self.y2_157.pos <- y2_0_145) ;
           (if self.i_160 then self.y1_156.pos <- y1_0_144) ;
           self.i_160 <- false ;
           (begin match z_146 with
                  | true ->
                      obsp_159 := true ;
                      obsv_158 := ((Distribution.draw (Distribution.gaussian 
                                                         (self.y1_156.pos ,
                                                          (( ** ) sigma_142 
                                                                  2.)))) ,
                                   (Distribution.draw (Distribution.gaussian 
                                                         (self.y2_157.pos ,
                                                          (( ** ) sigma_142 
                                                                  2.)))))
                  | _ -> ()  end) ;
           self.y2_157.der <- (-.) ((~-.) self.y1_156.pos) 
                                   (( *. ) theta_143  self.y2_157.pos) ;
           self.y1_156.der <- self.y2_157.pos ; (!obsv_158 , !obsp_159) in
       cpos_209 := cindex_207 ;
       (if cstate_206.major then
        (((Zls.set cstate_206.cvec  !cpos_209  self.y2_157.pos ;
           cpos_209 := (+) !cpos_209  1) ;
          (Zls.set cstate_206.cvec  !cpos_209  self.y1_156.pos ;
           cpos_209 := (+) !cpos_209  1)))
        else (((Zls.set cstate_206.dvec  !cpos_209  self.y2_157.der ;
                cpos_209 := (+) !cpos_209  1) ;
               (Zls.set cstate_206.dvec  !cpos_209  self.y1_156.der ;
                cpos_209 := (+) !cpos_209  1)))) ; result_211)):(float *
                                                                 float)
                                                                signal) in 
  let sho_noisy_reset self  =
    (self.i_160 <- true:unit) in
  Cnode { alloc = sho_noisy_alloc; copy = sho_noisy_copy ;
                                   step = sho_noisy_step ;
                                   reset = sho_noisy_reset }
type ('e , 'd , 'c , 'b , 'a) _sho_model =
  { mutable i_196 : 'e ;
    mutable major_168 : 'd ;
    mutable i_173 : 'c ; mutable theta_170 : 'b ; mutable sigma_169 : 'a }

let sho_model (cstate_212:Ztypes.cstate) = 
  let Cnode { alloc = i_196_alloc; copy = i_196_copy ;
                                   step = i_196_step ; reset = i_196_reset } = sho_sim 
  cstate_212 in
  let sho_model_alloc _ =
    ();
    { major_168 = false ;
      i_173 = (false:bool) ;
      theta_170 = (42.:float) ; sigma_169 = (42.:float);
      i_196 = i_196_alloc () (* continuous *)  } in
  let sho_model_copy source dest =
    dest.major_168 <- source.major_168 ;
    dest.i_173 <- source.i_173 ;
    dest.theta_170 <- source.theta_170 ; dest.sigma_169 <- source.sigma_169;
    i_196_copy source.i_196 dest.i_196 (* continuous *) in
  let sho_model_step self ((time_167:float) ,
                           ((prob_161:Infer_importance.prob) ,
                            ((((obsv_164:float) , (obsv_165:float)) ,
                              (obsp_166:bool)) ,
                             (y1_0_162:float) , (y2_0_163:float)))) =
    ((self.major_168 <- cstate_212.major ;
      ((if self.i_173 then
        self.sigma_169 <- Infer_importance.sample' (prob_161 ,
                                                    (Distribution.gaussian 
                                                       (0.5 , 0.1)))) ;
       (if self.i_173 then
        self.theta_170 <- Infer_importance.sample' (prob_161 ,
                                                    (Distribution.gaussian 
                                                       (0. , 0.5)))) ;
       self.i_173 <- false ;
       (let ((y1_171:float) , (y2_172:float)) =
            i_196_step self.i_196
              (time_167 , (self.theta_170 , y1_0_162 , y2_0_163)) in
        (begin match ((obsv_164 , obsv_165) , obsp_166) with
               | (((y1_obs_174:float) , (y2_obs_175:float)) , true) ->
                   let () =
                       Infer_importance.observe' (prob_161 ,
                                                  ((Distribution.gaussian 
                                                      (y2_172 ,
                                                       (( ** ) self.sigma_169
                                                                2.))) ,
                                                   y2_obs_175)) in
                   let () =
                       Infer_importance.observe' (prob_161 ,
                                                  ((Distribution.gaussian 
                                                      (y1_171 ,
                                                       (( ** ) self.sigma_169
                                                                2.))) ,
                                                   y1_obs_174)) in
                   () | _ -> ()  end) ; (self.theta_170 , self.sigma_169)))):
    float * float) in 
  let sho_model_reset self  =
    ((self.i_173 <- true ; i_196_reset self.i_196 ):unit) in
  Cnode { alloc = sho_model_alloc; copy = sho_model_copy ;
                                   step = sho_model_step ;
                                   reset = sho_model_reset }
type ('e , 'd , 'c , 'b , 'a) _horizon =
  { mutable major_178 : 'e ;
    mutable i_182 : 'd ;
    mutable x_181 : 'c ; mutable x_180 : 'b ; mutable t_179 : 'a }

let horizon (cstate_218:Ztypes.cstate) = 
  
  let horizon_alloc _ =
    cstate_218.cmax <- (+) cstate_218.cmax  1 ;
    cstate_218.zmax <- (+) cstate_218.zmax  2;
    { major_178 = false ;
      i_182 = (false:bool) ;
      x_181 = { zin = false; zout = 1. } ;
      x_180 = { zin = false; zout = 1. } ; t_179 = { pos = 42.; der = 0. } } in
  let horizon_copy source dest =
    dest.major_178 <- source.major_178 ;
    dest.i_182 <- source.i_182 ;
    dest.x_181.zin <- source.x_181.zin;dest.x_181.zout <- source.x_181.zout 
    ;
    dest.x_180.zin <- source.x_180.zin;dest.x_180.zout <- source.x_180.zout 
    ; dest.t_179.pos <- source.t_179.pos;dest.t_179.der <- source.t_179.der  in
  let horizon_step self ((time_177:float) , (h_176:float)) =
    ((let (cindex_219:int) = cstate_218.cindex in
      let cpos_221 = ref (cindex_219:int) in
      let (zindex_220:int) = cstate_218.zindex in
      let zpos_222 = ref (zindex_220:int) in
      cstate_218.cindex <- (+) cstate_218.cindex  1 ;
      cstate_218.zindex <- (+) cstate_218.zindex  2 ;
      self.major_178 <- cstate_218.major ;
      (if cstate_218.major then
       for i_1 = cindex_219 to 0 do Zls.set cstate_218.dvec  i_1  0. done
       else ((self.t_179.pos <- Zls.get cstate_218.cvec  !cpos_221 ;
              cpos_221 := (+) !cpos_221  1))) ;
      (let (result_223:zero) =
           (if self.i_182 then self.t_179.pos <- (~-.) h_176) ;
           self.i_182 <- false ;
           (begin match self.x_180.zin with
                  | true -> self.t_179.pos <- (~-.) h_176 | _ -> ()  end) ;
           self.x_180.zout <- self.t_179.pos ;
           self.t_179.der <- 1. ;
           self.x_181.zout <- self.t_179.pos ; self.x_181.zin in
       cpos_221 := cindex_219 ;
       (if cstate_218.major then
        (((Zls.set cstate_218.cvec  !cpos_221  self.t_179.pos ;
           cpos_221 := (+) !cpos_221  1)) ;
         ((self.x_181.zin <- false) ; (self.x_180.zin <- false)))
        else (((self.x_181.zin <- Zls.get_zin cstate_218.zinvec  !zpos_222 ;
                zpos_222 := (+) !zpos_222  1) ;
               (self.x_180.zin <- Zls.get_zin cstate_218.zinvec  !zpos_222 ;
                zpos_222 := (+) !zpos_222  1)) ;
              zpos_222 := zindex_220 ;
              ((Zls.set cstate_218.zoutvec  !zpos_222  self.x_181.zout ;
                zpos_222 := (+) !zpos_222  1) ;
               (Zls.set cstate_218.zoutvec  !zpos_222  self.x_180.zout ;
                zpos_222 := (+) !zpos_222  1)) ;
              ((Zls.set cstate_218.dvec  !cpos_221  self.t_179.der ;
                cpos_221 := (+) !cpos_221  1)))) ; result_223)):zero) in 
  let horizon_reset self  =
    (self.i_182 <- true:unit) in
  Cnode { alloc = horizon_alloc; copy = horizon_copy ;
                                 step = horizon_step ; reset = horizon_reset }
type ('d , 'c , 'b , 'a) _main =
  { mutable i_199 : 'd ;
    mutable i_198 : 'c ; mutable i_197 : 'b ; mutable major_184 : 'a }

let main (cstate_224:Ztypes.cstate) = 
  let Cnode { alloc = i_199_alloc; copy = i_199_copy ;
                                   step = i_199_step ; reset = i_199_reset } = sho_noisy 
  cstate_224 in 
  let Cnode { alloc = i_198_alloc; copy = i_198_copy ;
                                   step = i_198_step ; reset = i_198_reset } = horizon 
  cstate_224 in 
  let Cnode { alloc = i_197_alloc; copy = i_197_copy ;
                                   step = i_197_step ; reset = i_197_reset } = 
  Infer_importance.hybrid_infer 10000  sho_model cstate_224 in
  let main_alloc _ =
    ();
    { major_184 = false;
      i_199 = i_199_alloc () (* continuous *)  ;
      i_198 = i_198_alloc () (* continuous *)  ;
      i_197 = i_197_alloc () (* continuous *)  } in
  let main_copy source dest =
    dest.major_184 <- source.major_184;
    i_199_copy source.i_199 dest.i_199 (* continuous *) ;
    i_198_copy source.i_198 dest.i_198 (* continuous *) ;
    i_197_copy source.i_197 dest.i_197 (* continuous *) in
  let main_step self ((time_183:float) , ()) =
    ((self.major_184 <- cstate_224.major ;
      (let (((obsv_192:float) , (obsv_193:float)) , (copy_195:bool)) =
           i_199_step self.i_199
             (time_183 ,
              ((i_198_step self.i_198 (time_183 , 0.1)) ,
               (-0.15) , 0.1 , 1. , 0.)) in
       let (dist_187:((float  * float))Distribution.t) =
           i_197_step self.i_197
             (time_183 , (((obsv_192 , obsv_193) , copy_195) , 1. , 0.)) in
       let ((d_theta_186:(float)Distribution.t) ,
            (d_sigma_185:(float)Distribution.t)) =
           Distribution.split dist_187 in
       (begin match ((obsv_192 , obsv_193) , copy_195) with
              | (_ , true) ->
                  let _ = print_float (Distribution.mean_float d_theta_186) in
                  let _ = print_string " " in
                  let _ = print_float (Distribution.mean_float d_sigma_185) in
                  let () = print_newline () in
                  () | _ -> ()  end) ; ())):unit) in 
  let main_reset self  =
    ((i_199_reset self.i_199  ;
      i_198_reset self.i_198  ; i_197_reset self.i_197 ):unit) in
  Cnode { alloc = main_alloc; copy = main_copy ;
                              step = main_step ; reset = main_reset }
