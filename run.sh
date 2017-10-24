#!/bin/bash

# To run with the external teacher in the
# nominal-learning-ons repository

mkfifo qs ans
time stack exec NominalAngluin2 NomLStarCol > qs < ans &
../nominal-learning-orbitsets/external_teacher qs ans "$@"

rm qs ans
