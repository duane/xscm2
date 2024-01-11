import csv


class Register:
    def __init__(self, name, size):
        self.name = name
        self.size = size


class Register32(Register):
    def __init__(self, idx):
        assert idx < 32
        self.size = 32
        self.value = idx
        if self.value < 31:
            super().__init__("w" + str(self.value), self.size)
        super().__init__("wsp", self.size)


class Register64(Register):
    def __init__(self, idx):
        assert idx < 32
        self.size = 32
        self.value = idx
        if self.value < 30:
            super().__init__("x" + str(self.value), self.size)
        if self.value == 30:
            super().__init__("lr", self.size)
        super().__init__("sp", self.size)

    def from_str(self, str):
        if str == "sp":
            return Register64(31)
        if str == "lr":
            return Register64(30)
        elif str[0] == "x":
            return Register64(int(str[1:]))
        raise ValueError("Unknown register " + str)


class Immediate:
    def __init__(self, value):
        self.value = value


class NoSchemaError(Exception):
    pass


class Assembler:
    def __init__(self):
        self.bitfields = []
        with open("assets/AArch64_ops.csv") as f:
            reader = csv.DictReader(f)
            idx = 0
            for idx, row in enumerate(reader):
                try:
                    parsed = Bitfield(row)
                    self.bitfields.append((parsed.full_name(), parsed))
                    idx += 1
                except NoSchemaError:
                    next
        self.__names__ = None
        self.__types__ = None

    def lookup_memorder(self, i):
        swapped = swap32(i)
        for name, bitfield in self.bitfields:
            if bitfield.is_fit(swapped):
                return bitfield
        return None

    def count(self):
        return len(self.bitfields)

    def types(self):
        if self.__types__ is None:
            self.__types__ = {}
            for _, bitfield in self.bitfields:
                parametric_nodes = []
                for node in bitfield.seq:
                    if isinstance(node, NamedBitfieldOperand):
                        parametric_nodes.append(node)
                signature = "".join(str(segment.fmt()) for segment in parametric_nodes)
                if signature == "":
                    continue
                if signature not in self.__types__:
                    self.__types__[signature] = 0
                self.__types__[signature] += 1
        return self.__types__

    def names(self):
        if self.__names__ is None:
            self.__names__ = set()
            for _, bitfield in self.bitfields:
                for node in bitfield.seq:
                    if isinstance(node, NamedBitfieldOperand):
                        self.__names__.add(node.name)
        return self.__names__

    def parse_token(self, token):
        if token[0] == "w":
            reg_num = int(token[1:])
            if reg_num < 31:
                return Register32(reg_num)
        if token[0] == "x":
            reg_num = int(token[1:])
            if reg_num < 30:
                return Register64(reg_num)
        if token == "sp":
            return Register64(31)
        if token == "lr":
            return Register64(30)
        if token == "wsp":
            return Register32(31)
        if token[0].isdigit() or token[0] == "-":
            return Immediate(int(token[1:]))
        raise ValueError("Unknown token " + token)

    def variants(self):
        variants = {}
        for name, bitfield in self.bitfields:
            if bitfield.variant == "":
                continue
            if bitfield.variant not in variants:
                variants[bitfield.variant] = 1
            else:
                variants[bitfield.variant] += 1
        return variants

    def assemble(self, insn_name, tokens):
        for bitfield in self.find_bitfields(insn_name.lower()):
            print(bitfield.full_name())
            if bitfield.matches_tokens(tokens):
                print(
                    "matched",
                    bitfield.full_name(),
                    "".join(str(s) for s in bitfield.fmt()),
                )
                result = bitfield.asm(tokens)
                print("result", hex(swap32(result)))
                return result
        raise ValueError("No matching bitfield found")

    def find_bitfields(self, insn_name):
        lowered_name = insn_name.lower()
        return [
            bitfield
            for (_, bitfield) in self.bitfields
            if bitfield.full_name().lower() == lowered_name
        ]

    def find_bitfield(self, insn):
        found = [
            (name, bitfield)
            for (name, bitfield) in self.bitfields
            if bitfield.is_fit(insn)
        ]
        if len(found) == 0:
            return None
        if len(found) == 1:
            return found[0]
        raise ValueError("Multiple matches found")


class Bitfield:
    def __init__(self, row):
        schema = row[
            "31:30:29:28:27:26:25:24:23:22:21:20:19:18:17:16:15:14:13:12:11:10:9:8:7:6:5:4:3:2:1:0"
        ]
        if schema == "":
            raise NoSchemaError
        self.name = row["Opcode"]
        self.variant = row["variant"]
        self.prependage = row["prependage"]
        self.specific = row["Specific"]
        self.appendage = row["appendage"]
        cur_node = None
        seq = []
        for idx, bit in enumerate(schema.split(":")):
            idx = 31 - idx
            if bit == "-":
                bit_value = row[str(idx)]
                if bit_value == "1":
                    bit_value = 1
                else:
                    bit_value = 0
                if cur_node and isinstance(cur_node, ConstantBitfieldOperand):
                    cur_node.add_bit_right(bit_value, idx)
                else:
                    if cur_node:
                        seq.append(cur_node)
                        cur_node = None
                    cur_node = ConstantBitfieldOperand(idx, idx, bit_value)
            elif bit == "":
                assert cur_node is not None
                assert isinstance(cur_node, NamedBitfieldOperand)
                cur_node.grow_right(idx)
            else:
                if cur_node:
                    seq.append(cur_node)
                    cur_node = None
                cur_node = NamedBitfieldOperand(idx, bit)
        assert cur_node is not None
        seq.append(cur_node)
        self.seq = seq
        self.__full_name__ = None

    def full_name(self):
        if self.__full_name__ is None:
            parts = [self.prependage, self.name.lower()]
            # if self.specific != "" and self.specific is not None:
            # parts.append(self.specific)
            self.__full_name__ = "".join(parts)
            if self.appendage:
                self.__full_name__ += "/" + self.appendage
        return self.__full_name__

    def fmt(self):
        for arg in self.seq:
            if isinstance(arg, NamedBitfieldOperand):
                yield (arg.name, arg.size())
            elif isinstance(arg, ConstantBitfieldOperand):
                yield (arg.value, arg.size())
            else:
                raise ValueError("Unknown type")

    def asm(self, args):
        val = 0
        last_idx = 0
        arg_idx = 0
        for arg in self.seq:
            if isinstance(arg, NamedBitfieldOperand):
                val = val + (args[arg_idx].value << arg.end)
                arg_idx += 1
                last_idx = arg.end
            elif isinstance(arg, ConstantBitfieldOperand):
                val = val + (arg.value << arg.end)
                last_idx = arg.end
            else:
                raise ValueError("Unknown type")
        assert last_idx == 0
        return val

    def disassemble(self, insn):
        assert self.is_fit(insn)
        args = []
        for param in self.seq:
            if isinstance(param, NamedBitfieldOperand):
                args.append((param.name, (insn >> param.end) & (2 ** param.size() - 1)))
        return args

    # returns true iff the constant fields match the passed instruction
    def is_fit(self, insn):
        for arg in self.seq:
            if isinstance(arg, ConstantBitfieldOperand):
                if not arg.test(insn):
                    return False
            else:
                continue
        return True

    def matches_tokens(self, tokens):
        token_idx = 0
        for field in self.seq:
            if isinstance(field, ConstantBitfieldOperand):
                continue
            if isinstance(field, NamedBitfieldOperand):
                tok = tokens[token_idx]
                token_idx += 1
                if not field.is_match(tok, self.variant):
                    return False
                continue
            raise ValueError("Unknown operand type")
        assert token_idx == len(tokens)
        return True


class BitfieldOperand:
    def __init__(self, start, end):
        self.start = start
        self.end = end

    def size(self):
        return self.start - self.end + 1

    def is_match(self, token, variant):
        return False


class NamedBitfieldOperand(BitfieldOperand):
    def __init__(self, start, name):
        self.start = start
        self.name = name
        self.end = start

    def grow_right(self, idx):
        assert idx == self.end - 1
        self.end = idx

    def fmt(self):
        return (self.name, self.start, self.size())

    def is_match(self, token, variant):
        if self.name == "imm7":
            if isinstance(token, Immediate):
                return token.value < 2**7 and token.value >= -(2**6)
            return False
        if self.name == "imm12":
            if isinstance(token, Immediate):
                return token.value < 2**12 and token.value >= -(2**11)
            return False
        elif self.name in ["Rt", "Rt2", "Rn", "Rd"]:
            if isinstance(token, Register64):
                return variant == "X"
            elif isinstance(token, Register32):
                return variant == "W"
            return False
        return False


class ConstantBitfieldOperand(BitfieldOperand):
    def __init__(self, start, end, value):
        self.start = start
        self.end = end
        self.value = value

    def add_bit_right(self, bit, idx):
        assert idx == self.end - 1
        assert bit in [0, 1]
        self.value = (self.value << 1) + bit
        self.end = idx

    def test(self, insn):
        return (insn >> self.end) & (2 ** self.size() - 1) == self.value

    def is_match(self, token):
        return False


import struct


def swap32(i):
    return struct.unpack("<I", struct.pack(">I", i))[0]


if __name__ == "__main__":
    asm = Assembler()
    insn = swap32(0xFD7BBFA9)
    (mnemonic, bitfield) = asm.find_bitfield(insn)
    result = bitfield.asm(
        [Immediate(126), Register64(30), Register64(31), Register64(29)]
    )
    assert result == insn
    result = asm.assemble("add/immx", [Immediate(0), Register64(31), Register64(30)])
