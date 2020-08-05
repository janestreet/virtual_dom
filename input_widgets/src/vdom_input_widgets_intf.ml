open Core_kernel

module type Display = sig
  type t

  val to_string : t -> string
end

module type Equal = sig
  type t [@@deriving equal]

  include Display with type t := t
end

module type Enum = sig
  type t [@@deriving equal, enumerate]

  include Display with type t := t
end

module type Set = sig
  type t
  type comparator_witness

  include Comparator.S with type t := t and type comparator_witness := comparator_witness
  include Display with type t := t
end

module type Enum_set = sig
  type t [@@deriving enumerate]
  type comparator_witness

  include Comparator.S with type t := t and type comparator_witness := comparator_witness
  include Enum with type t := t
end
