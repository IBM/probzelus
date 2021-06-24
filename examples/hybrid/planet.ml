(* The Zelus compiler, version 2.2-dev
  (2021-06-18-7:42) *)
open Ztypes
open Probzelus
open Infer_importance
open Distribution
let m = 1.

let q0x = 1.

let q0y = 0.

let p0x = 0.

let p0y = 1.

let sigma = 0.01

type ('f , 'e , 'd , 'c , 'b , 'a) _planet_sim =
  { mutable major_98 : 'f ;
    mutable i_104 : 'e ;
    mutable qy_102 : 'd ;
    mutable qx_101 : 'c ; mutable py_100 : 'b ; mutable px_99 : 'a }

let planet_sim (cstate_150:Ztypes.cstate) = 
  
  let planet_sim_alloc _ =
    cstate_150.cmax <- (+) cstate_150.cmax  4;
    { major_98 = false ;
      i_104 = (false:bool) ;
      qy_102 = { pos = 42.; der = 0. } ;
      qx_101 = { pos = 42.; der = 0. } ;
      py_100 = { pos = 42.; der = 0. } ; px_99 = { pos = 42.; der = 0. } } in
  let planet_sim_copy source dest =
    dest.major_98 <- source.major_98 ;
    dest.i_104 <- source.i_104 ;
    dest.qy_102.pos <- source.qy_102.pos;
    dest.qy_102.der <- source.qy_102.der  ;
    dest.qx_101.pos <- source.qx_101.pos;
    dest.qx_101.der <- source.qx_101.der  ;
    dest.py_100.pos <- source.py_100.pos;
    dest.py_100.der <- source.py_100.der  ;
    dest.px_99.pos <- source.px_99.pos;dest.px_99.der <- source.px_99.der  in
  let planet_sim_step self ((time_97:float) , (k_96:float)) =
    ((let (cindex_151:int) = cstate_150.cindex in
      let cpos_153 = ref (cindex_151:int) in
      cstate_150.cindex <- (+) cstate_150.cindex  4 ;
      self.major_98 <- cstate_150.major ;
      (if cstate_150.major then
       for i_1 = cindex_151 to 3 do Zls.set cstate_150.dvec  i_1  0. done
       else ((self.qy_102.pos <- Zls.get cstate_150.cvec  !cpos_153 ;
              cpos_153 := (+) !cpos_153  1) ;
             (self.qx_101.pos <- Zls.get cstate_150.cvec  !cpos_153 ;
              cpos_153 := (+) !cpos_153  1) ;
             (self.py_100.pos <- Zls.get cstate_150.cvec  !cpos_153 ;
              cpos_153 := (+) !cpos_153  1) ;
             (self.px_99.pos <- Zls.get cstate_150.cvec  !cpos_153 ;
              cpos_153 := (+) !cpos_153  1))) ;
      (let (result_155:(float  * float)) =
           (if self.i_104 then self.py_100.pos <- p0y) ;
           (if self.i_104 then self.px_99.pos <- p0x) ;
           (if self.i_104 then self.qy_102.pos <- q0y) ;
           (if self.i_104 then self.qx_101.pos <- q0x) ;
           self.i_104 <- false ;
           (let (r_cube_103:float) =
                ( ** ) ((+.) (( ** ) self.qx_101.pos  2.) 
                             (( ** ) self.qy_102.pos  2.))  1.5 in
            self.py_100.der <- (/.) (( *. ) ((~-.) k_96)  self.qy_102.pos) 
                                    r_cube_103 ;
            self.px_99.der <- (/.) (( *. ) ((~-.) k_96)  self.qx_101.pos) 
                                   r_cube_103 ;
            self.qy_102.der <- (/.) self.py_100.pos  m ;
            self.qx_101.der <- (/.) self.px_99.pos  m ;
            (self.qx_101.pos , self.qy_102.pos)) in
       cpos_153 := cindex_151 ;
       (if cstate_150.major then
        (((Zls.set cstate_150.cvec  !cpos_153  self.qy_102.pos ;
           cpos_153 := (+) !cpos_153  1) ;
          (Zls.set cstate_150.cvec  !cpos_153  self.qx_101.pos ;
           cpos_153 := (+) !cpos_153  1) ;
          (Zls.set cstate_150.cvec  !cpos_153  self.py_100.pos ;
           cpos_153 := (+) !cpos_153  1) ;
          (Zls.set cstate_150.cvec  !cpos_153  self.px_99.pos ;
           cpos_153 := (+) !cpos_153  1)))
        else (((Zls.set cstate_150.dvec  !cpos_153  self.qy_102.der ;
                cpos_153 := (+) !cpos_153  1) ;
               (Zls.set cstate_150.dvec  !cpos_153  self.qx_101.der ;
                cpos_153 := (+) !cpos_153  1) ;
               (Zls.set cstate_150.dvec  !cpos_153  self.py_100.der ;
                cpos_153 := (+) !cpos_153  1) ;
               (Zls.set cstate_150.dvec  !cpos_153  self.px_99.der ;
                cpos_153 := (+) !cpos_153  1)))) ; result_155)):float * float) in
   let planet_sim_reset self  =
     (self.i_104 <- true:unit) in
  Cnode { alloc = planet_sim_alloc; copy = planet_sim_copy ;
                                    step = planet_sim_step ;
                                    reset = planet_sim_reset }
type ('f , 'e , 'd , 'c , 'b , 'a) _planet_noisy =
  { mutable major_108 : 'f ;
    mutable i_119 : 'e ;
    mutable qy_115 : 'd ;
    mutable qx_114 : 'c ; mutable py_113 : 'b ; mutable px_112 : 'a }

let planet_noisy (cstate_156:Ztypes.cstate) = 
  
  let planet_noisy_alloc _ =
    cstate_156.cmax <- (+) cstate_156.cmax  4;
    { major_108 = false ;
      i_119 = (false:bool) ;
      qy_115 = { pos = 42.; der = 0. } ;
      qx_114 = { pos = 42.; der = 0. } ;
      py_113 = { pos = 42.; der = 0. } ; px_112 = { pos = 42.; der = 0. } } in
  let planet_noisy_copy source dest =
    dest.major_108 <- source.major_108 ;
    dest.i_119 <- source.i_119 ;
    dest.qy_115.pos <- source.qy_115.pos;
    dest.qy_115.der <- source.qy_115.der  ;
    dest.qx_114.pos <- source.qx_114.pos;
    dest.qx_114.der <- source.qx_114.der  ;
    dest.py_113.pos <- source.py_113.pos;
    dest.py_113.der <- source.py_113.der  ;
    dest.px_112.pos <- source.px_112.pos;
    dest.px_112.der <- source.px_112.der  in
  let planet_noisy_step self ((time_107:float) ,
                              ((z_106:zero) , (k_105:float))) =
    ((let (cindex_157:int) = cstate_156.cindex in
      let cpos_159 = ref (cindex_157:int) in
      cstate_156.cindex <- (+) cstate_156.cindex  4 ;
      self.major_108 <- cstate_156.major ;
      (if cstate_156.major then
       for i_1 = cindex_157 to 3 do Zls.set cstate_156.dvec  i_1  0. done
       else ((self.qy_115.pos <- Zls.get cstate_156.cvec  !cpos_159 ;
              cpos_159 := (+) !cpos_159  1) ;
             (self.qx_114.pos <- Zls.get cstate_156.cvec  !cpos_159 ;
              cpos_159 := (+) !cpos_159  1) ;
             (self.py_113.pos <- Zls.get cstate_156.cvec  !cpos_159 ;
              cpos_159 := (+) !cpos_159  1) ;
             (self.px_112.pos <- Zls.get cstate_156.cvec  !cpos_159 ;
              cpos_159 := (+) !cpos_159  1))) ;
      (let (result_161:((float  * float))signal) =
           let obsp_118 = ref (false:bool) in
           let obsv_117 = ref ((42. , 42.):float * float) in
           (if self.i_119 then self.py_113.pos <- p0y) ;
           (if self.i_119 then self.px_112.pos <- p0x) ;
           (if self.i_119 then self.qy_115.pos <- q0y) ;
           (if self.i_119 then self.qx_114.pos <- q0x) ;
           self.i_119 <- false ;
           (begin match z_106 with
                  | true ->
                      obsp_118 := true ;
                      obsv_117 := ((Distribution.draw (Distribution.gaussian 
                                                         (self.qx_114.pos ,
                                                          sigma))) ,
                                   (Distribution.draw (Distribution.gaussian 
                                                         (self.qy_115.pos ,
                                                          sigma))))
                  | _ -> ()  end) ;
           (let (r_cube_116:float) =
                ( ** ) ((+.) (( ** ) self.qx_114.pos  2.) 
                             (( ** ) self.qy_115.pos  2.))  1.5 in
            self.py_113.der <- (/.) (( *. ) ((~-.) k_105)  self.qy_115.pos) 
                                    r_cube_116 ;
            self.px_112.der <- (/.) (( *. ) ((~-.) k_105)  self.qx_114.pos) 
                                    r_cube_116 ;
            self.qy_115.der <- (/.) self.py_113.pos  m ;
            self.qx_114.der <- (/.) self.px_112.pos  m ;
            (!obsv_117 , !obsp_118)) in
       cpos_159 := cindex_157 ;
       (if cstate_156.major then
        (((Zls.set cstate_156.cvec  !cpos_159  self.qy_115.pos ;
           cpos_159 := (+) !cpos_159  1) ;
          (Zls.set cstate_156.cvec  !cpos_159  self.qx_114.pos ;
           cpos_159 := (+) !cpos_159  1) ;
          (Zls.set cstate_156.cvec  !cpos_159  self.py_113.pos ;
           cpos_159 := (+) !cpos_159  1) ;
          (Zls.set cstate_156.cvec  !cpos_159  self.px_112.pos ;
           cpos_159 := (+) !cpos_159  1)))
        else (((Zls.set cstate_156.dvec  !cpos_159  self.qy_115.der ;
                cpos_159 := (+) !cpos_159  1) ;
               (Zls.set cstate_156.dvec  !cpos_159  self.qx_114.der ;
                cpos_159 := (+) !cpos_159  1) ;
               (Zls.set cstate_156.dvec  !cpos_159  self.py_113.der ;
                cpos_159 := (+) !cpos_159  1) ;
               (Zls.set cstate_156.dvec  !cpos_159  self.px_112.der ;
                cpos_159 := (+) !cpos_159  1)))) ; result_161)):(float *
                                                                 float)
                                                                signal) in 
  let planet_noisy_reset self  =
    (self.i_119 <- true:unit) in
  Cnode { alloc = planet_noisy_alloc; copy = planet_noisy_copy ;
                                      step = planet_noisy_step ;
                                      reset = planet_noisy_reset }
type ('d , 'c , 'b , 'a) _planet_model =
  { mutable i_146 : 'd ;
    mutable major_125 : 'c ; mutable i_129 : 'b ; mutable k_126 : 'a }

let planet_model (cstate_162:Ztypes.cstate) = 
  let Cnode { alloc = i_146_alloc; copy = i_146_copy ;
                                   step = i_146_step ; reset = i_146_reset } = planet_sim 
  cstate_162 in
  let planet_model_alloc _ =
    ();
    { major_125 = false ; i_129 = (false:bool) ; k_126 = (42.:float);
      i_146 = i_146_alloc () (* continuous *)  } in
  let planet_model_copy source dest =
    dest.major_125 <- source.major_125 ;
    dest.i_129 <- source.i_129 ; dest.k_126 <- source.k_126;
    i_146_copy source.i_146 dest.i_146 (* continuous *) in
  let planet_model_step self ((time_124:float) ,
                              ((prob_120:Infer_importance.prob) ,
                               (((obsv_121:float) , (obsv_122:float)) ,
                                (obsp_123:bool)))) =
    ((self.major_125 <- cstate_162.major ;
      ((if self.i_129 then
        self.k_126 <- Infer_importance.sample' (prob_120 ,
                                                (Distribution.gaussian 
                                                   (0. , 1.)))) ;
       self.i_129 <- false ;
       (let ((qx_127:float) , (qy_128:float)) =
            i_146_step self.i_146 (time_124 , self.k_126) in
        (begin match ((obsv_121 , obsv_122) , obsp_123) with
               | (((qx_obs_130:float) , (qy_obs_131:float)) , true) ->
                   let () =
                       Infer_importance.observe' (prob_120 ,
                                                  ((Distribution.gaussian 
                                                      (qy_128 , sigma)) ,
                                                   qy_obs_131)) in
                   let () =
                       Infer_importance.observe' (prob_120 ,
                                                  ((Distribution.gaussian 
                                                      (qx_127 , sigma)) ,
                                                   qx_obs_130)) in
                   () | _ -> ()  end) ; self.k_126))):float) in 
  let planet_model_reset self  =
    ((self.i_129 <- true ; i_146_reset self.i_146 ):unit) in
  Cnode { alloc = planet_model_alloc; copy = planet_model_copy ;
                                      step = planet_model_step ;
                                      reset = planet_model_reset }
type ('e , 'd , 'c , 'b , 'a) _horizon =
  { mutable major_134 : 'e ;
    mutable i_138 : 'd ;
    mutable x_137 : 'c ; mutable x_136 : 'b ; mutable t_135 : 'a }

let horizon (cstate_168:Ztypes.cstate) = 
  
  let horizon_alloc _ =
    cstate_168.cmax <- (+) cstate_168.cmax  1 ;
    cstate_168.zmax <- (+) cstate_168.zmax  2;
    { major_134 = false ;
      i_138 = (false:bool) ;
      x_137 = { zin = false; zout = 1. } ;
      x_136 = { zin = false; zout = 1. } ; t_135 = { pos = 42.; der = 0. } } in
  let horizon_copy source dest =
    dest.major_134 <- source.major_134 ;
    dest.i_138 <- source.i_138 ;
    dest.x_137.zin <- source.x_137.zin;dest.x_137.zout <- source.x_137.zout 
    ;
    dest.x_136.zin <- source.x_136.zin;dest.x_136.zout <- source.x_136.zout 
    ; dest.t_135.pos <- source.t_135.pos;dest.t_135.der <- source.t_135.der  in
  let horizon_step self ((time_133:float) , (h_132:float)) =
    ((let (cindex_169:int) = cstate_168.cindex in
      let cpos_171 = ref (cindex_169:int) in
      let (zindex_170:int) = cstate_168.zindex in
      let zpos_172 = ref (zindex_170:int) in
      cstate_168.cindex <- (+) cstate_168.cindex  1 ;
      cstate_168.zindex <- (+) cstate_168.zindex  2 ;
      self.major_134 <- cstate_168.major ;
      (if cstate_168.major then
       for i_1 = cindex_169 to 0 do Zls.set cstate_168.dvec  i_1  0. done
       else ((self.t_135.pos <- Zls.get cstate_168.cvec  !cpos_171 ;
              cpos_171 := (+) !cpos_171  1))) ;
      (let (result_173:zero) =
           (if self.i_138 then self.t_135.pos <- (~-.) h_132) ;
           self.i_138 <- false ;
           (begin match self.x_136.zin with
                  | true -> self.t_135.pos <- (~-.) h_132 | _ -> ()  end) ;
           self.x_136.zout <- self.t_135.pos ;
           self.t_135.der <- 1. ;
           self.x_137.zout <- self.t_135.pos ; self.x_137.zin in
       cpos_171 := cindex_169 ;
       (if cstate_168.major then
        (((Zls.set cstate_168.cvec  !cpos_171  self.t_135.pos ;
           cpos_171 := (+) !cpos_171  1)) ;
         ((self.x_137.zin <- false) ; (self.x_136.zin <- false)))
        else (((self.x_137.zin <- Zls.get_zin cstate_168.zinvec  !zpos_172 ;
                zpos_172 := (+) !zpos_172  1) ;
               (self.x_136.zin <- Zls.get_zin cstate_168.zinvec  !zpos_172 ;
                zpos_172 := (+) !zpos_172  1)) ;
              zpos_172 := zindex_170 ;
              ((Zls.set cstate_168.zoutvec  !zpos_172  self.x_137.zout ;
                zpos_172 := (+) !zpos_172  1) ;
               (Zls.set cstate_168.zoutvec  !zpos_172  self.x_136.zout ;
                zpos_172 := (+) !zpos_172  1)) ;
              ((Zls.set cstate_168.dvec  !cpos_171  self.t_135.der ;
                cpos_171 := (+) !cpos_171  1)))) ; result_173)):zero) in 
  let horizon_reset self  =
    (self.i_138 <- true:unit) in
  Cnode { alloc = horizon_alloc; copy = horizon_copy ;
                                 step = horizon_step ; reset = horizon_reset }
type ('d , 'c , 'b , 'a) _main =
  { mutable i_149 : 'd ;
    mutable i_148 : 'c ; mutable i_147 : 'b ; mutable major_140 : 'a }

let main (cstate_174:Ztypes.cstate) = 
  let Cnode { alloc = i_149_alloc; copy = i_149_copy ;
                                   step = i_149_step ; reset = i_149_reset } = planet_noisy 
  cstate_174 in 
  let Cnode { alloc = i_148_alloc; copy = i_148_copy ;
                                   step = i_148_step ; reset = i_148_reset } = horizon 
  cstate_174 in 
  let Cnode { alloc = i_147_alloc; copy = i_147_copy ;
                                   step = i_147_step ; reset = i_147_reset } = 
  Infer_importance.hybrid_infer 10000  planet_model cstate_174 in
  let main_alloc _ =
    ();
    { major_140 = false;
      i_149 = i_149_alloc () (* continuous *)  ;
      i_148 = i_148_alloc () (* continuous *)  ;
      i_147 = i_147_alloc () (* continuous *)  } in
  let main_copy source dest =
    dest.major_140 <- source.major_140;
    i_149_copy source.i_149 dest.i_149 (* continuous *) ;
    i_148_copy source.i_148 dest.i_148 (* continuous *) ;
    i_147_copy source.i_147 dest.i_147 (* continuous *) in
  let main_step self ((time_139:float) , ()) =
    ((self.major_140 <- cstate_174.major ;
      (let (((obsv_142:float) , (obsv_143:float)) , (copy_145:bool)) =
           i_149_step self.i_149
             (time_139 , ((i_148_step self.i_148 (time_139 , 0.1)) , 1.2)) in
       let (d_141:(float)Distribution.t) =
           i_147_step self.i_147
             (time_139 , ((obsv_142 , obsv_143) , copy_145)) in
       (begin match ((obsv_142 , obsv_143) , copy_145) with
              | (_ , true) ->
                  let _ = print_float (Distribution.mean_float d_141) in
                  let () = print_newline () in
                  () | _ -> ()  end) ; ())):unit) in 
  let main_reset self  =
    ((i_149_reset self.i_149  ;
      i_148_reset self.i_148  ; i_147_reset self.i_147 ):unit) in
  Cnode { alloc = main_alloc; copy = main_copy ;
                              step = main_step ; reset = main_reset }
