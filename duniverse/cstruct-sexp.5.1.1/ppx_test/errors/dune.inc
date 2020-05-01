
(rule
  (deps pp.exe (:input cenum_id_payload.ml))
  (targets cenum_id_payload.ml.errors)
  (action
    (with-stderr-to
      %{targets}
      (run ./pp.exe --impl %{input}))))

(alias
  (name runtest)
  (package ppx_cstruct)
  (action
    (diff cenum_id_payload.ml.expected cenum_id_payload.ml.errors)))

(rule
  (deps pp.exe (:input cenum_invalid_type.ml))
  (targets cenum_invalid_type.ml.errors)
  (action
    (with-stderr-to
      %{targets}
      (run ./pp.exe --impl %{input}))))

(alias
  (name runtest)
  (package ppx_cstruct)
  (action
    (diff cenum_invalid_type.ml.expected cenum_invalid_type.ml.errors)))

(rule
  (deps pp.exe (:input cenum_no_attribute.ml))
  (targets cenum_no_attribute.ml.errors)
  (action
    (with-stderr-to
      %{targets}
      (run ./pp.exe --impl %{input}))))

(alias
  (name runtest)
  (package ppx_cstruct)
  (action
    (diff cenum_no_attribute.ml.expected cenum_no_attribute.ml.errors)))

(rule
  (deps pp.exe (:input cenum_not_a_variant.ml))
  (targets cenum_not_a_variant.ml.errors)
  (action
    (with-stderr-to
      %{targets}
      (run ./pp.exe --impl %{input}))))

(alias
  (name runtest)
  (package ppx_cstruct)
  (action
    (diff cenum_not_a_variant.ml.expected cenum_not_a_variant.ml.errors)))

(rule
  (deps pp.exe (:input cenum_unknown_attribute.ml))
  (targets cenum_unknown_attribute.ml.errors)
  (action
    (with-stderr-to
      %{targets}
      (run ./pp.exe --impl %{input}))))

(alias
  (name runtest)
  (package ppx_cstruct)
  (action
    (diff cenum_unknown_attribute.ml.expected cenum_unknown_attribute.ml.errors)))

(rule
  (deps pp.exe (:input cstruct_attribute_payload.ml))
  (targets cstruct_attribute_payload.ml.errors)
  (action
    (with-stderr-to
      %{targets}
      (run ./pp.exe --impl %{input}))))

(alias
  (name runtest)
  (package ppx_cstruct)
  (action
    (diff cstruct_attribute_payload.ml.expected cstruct_attribute_payload.ml.errors)))

(rule
  (deps pp.exe (:input cstruct_duplicate_field.ml))
  (targets cstruct_duplicate_field.ml.errors)
  (action
    (with-stderr-to
      %{targets}
      (run ./pp.exe --impl %{input}))))

(alias
  (name runtest)
  (package ppx_cstruct)
  (action
    (diff cstruct_duplicate_field.ml.expected cstruct_duplicate_field.ml.errors)))

(rule
  (deps pp.exe (:input cstruct_len_int32.ml))
  (targets cstruct_len_int32.ml.errors)
  (action
    (with-stderr-to
      %{targets}
      (run ./pp.exe --impl %{input}))))

(alias
  (name runtest)
  (package ppx_cstruct)
  (action
    (diff cstruct_len_int32.ml.expected cstruct_len_int32.ml.errors)))

(rule
  (deps pp.exe (:input cstruct_len_not_int.ml))
  (targets cstruct_len_not_int.ml.errors)
  (action
    (with-stderr-to
      %{targets}
      (run ./pp.exe --impl %{input}))))

(alias
  (name runtest)
  (package ppx_cstruct)
  (action
    (diff cstruct_len_not_int.ml.expected cstruct_len_not_int.ml.errors)))

(rule
  (deps pp.exe (:input cstruct_len_zero.ml))
  (targets cstruct_len_zero.ml.errors)
  (action
    (with-stderr-to
      %{targets}
      (run ./pp.exe --impl %{input}))))

(alias
  (name runtest)
  (package ppx_cstruct)
  (action
    (diff cstruct_len_zero.ml.expected cstruct_len_zero.ml.errors)))

(rule
  (deps pp.exe (:input cstruct_multiple_len.ml))
  (targets cstruct_multiple_len.ml.errors)
  (action
    (with-stderr-to
      %{targets}
      (run ./pp.exe --impl %{input}))))

(alias
  (name runtest)
  (package ppx_cstruct)
  (action
    (diff cstruct_multiple_len.ml.expected cstruct_multiple_len.ml.errors)))

(rule
  (deps pp.exe (:input cstruct_not_a_record.ml))
  (targets cstruct_not_a_record.ml.errors)
  (action
    (with-stderr-to
      %{targets}
      (run ./pp.exe --impl %{input}))))

(alias
  (name runtest)
  (package ppx_cstruct)
  (action
    (diff cstruct_not_a_record.ml.expected cstruct_not_a_record.ml.errors)))

(rule
  (deps pp.exe (:input cstruct_not_an_identifier.ml))
  (targets cstruct_not_an_identifier.ml.errors)
  (action
    (with-stderr-to
      %{targets}
      (run ./pp.exe --impl %{input}))))

(alias
  (name runtest)
  (package ppx_cstruct)
  (action
    (diff cstruct_not_an_identifier.ml.expected cstruct_not_an_identifier.ml.errors)))

(rule
  (deps pp.exe (:input cstruct_several_attributes.ml))
  (targets cstruct_several_attributes.ml.errors)
  (action
    (with-stderr-to
      %{targets}
      (run ./pp.exe --impl %{input}))))

(alias
  (name runtest)
  (package ppx_cstruct)
  (action
    (diff cstruct_several_attributes.ml.expected cstruct_several_attributes.ml.errors)))

(rule
  (deps pp.exe (:input cstruct_unknown_endian.ml))
  (targets cstruct_unknown_endian.ml.errors)
  (action
    (with-stderr-to
      %{targets}
      (run ./pp.exe --impl %{input}))))

(alias
  (name runtest)
  (package ppx_cstruct)
  (action
    (diff cstruct_unknown_endian.ml.expected cstruct_unknown_endian.ml.errors)))

(rule
  (deps pp.exe (:input cstruct_unknown_type.ml))
  (targets cstruct_unknown_type.ml.errors)
  (action
    (with-stderr-to
      %{targets}
      (run ./pp.exe --impl %{input}))))

(alias
  (name runtest)
  (package ppx_cstruct)
  (action
    (diff cstruct_unknown_type.ml.expected cstruct_unknown_type.ml.errors)))
