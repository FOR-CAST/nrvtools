# .age_class_raster validates lengths

    Code
      nrvtools:::.age_class_raster(mk_age(matrix(1L, 2, 2), 10), ageClassCutOffs = c(
        0, 40), ageClasses = acl)
    Condition
      Error:
      ! `ageClassCutOffs` must be the same length as `ageClasses`.

