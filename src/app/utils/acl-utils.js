const MASKS = {
  read_object:          0x00000001,
  list_container:       0x00000001,
  write_object:         0x00000002,
  add_object:           0x00000002,
  append_data:          0x00000004,
  add_subcontainer:     0x00000004,
  read_metadata:        0x00000008,
  write_metadata:       0x00000010,
  execute:              0x00000020,
  traverse_container:   0x00000020,
  delete_object:        0x00000040,
  delete_subcontainer:  0x00000040,
  read_attributes:      0x00000080,
  write_attributes:     0x00000100,
  delete:               0x00010000,
  read_acl:             0x00020000,
  write_acl:            0x00040000,
  write_owner:          0x00080000,
};

function setAclFlag(permissions, flagName, value) {
  if (value === undefined) {
    value = true;
  }
  const mask = MASKS[flagName];
  return value ?
    (permissions | mask) : (permissions & ~mask);
}

function getAclFlag(permissions, flagName) {
  const mask = MASKS[flagName];
  return (permissions & mask) === mask;
}

export { MASKS, setAclFlag, getAclFlag };
