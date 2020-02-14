(*
 * Copyright 2018-2020 IBM Corporation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

include Owl.Mat

let uniform a b x y = uniform ~a ~b x y

let create_ out = create_ ~out

let uniform_ a b out = uniform_ ~a ~b ~out

let bernoulli_ out = bernoulli_ ~out

let zeros_ out = zeros_ ~out

let ones_ out = ones_ ~out

let one_hot_ out = one_hot_ ~out

let copy_ out = copy_ ~out

let reshape_ out = reshape_ ~out

let transpose_ out = transpose_ ~out

let sum_ out = sum_ ~out

let min_ out = min_ ~out

let max_ out = max_ ~out

let dot_ ~c = dot_ ~c
