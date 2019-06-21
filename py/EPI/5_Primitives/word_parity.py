"""
The parity of a binary word is 1 if the number of 1s in the word is odd; otherwise, it is 0. For example, the parity of
1011 is 1, and the parity of 10001000 is 0. Parity checks are used to detect single bit errors in data storage and
communication. It is fairly straightforward to write code that computes the parity of a single 64-bit word.

How would you compute the parity of a very large number of 64-bit words?

Hint: Use a lookup table, but don't use 2^64 entries.
"""

# ---------------------------------------------------------------
# First Approach
# ---------------------------------------------------------------

# Probably some traversal applying a variant of Kernighan's algorithm for O(log b) find the number of set bits (1s)
# where b is 64. Instead of checking for a isOdd/Even after counting set bits, toggle a flag while counting.


def shitty_count_set_bits(num:int) -> int:
    """
    O(n) - use kernighan's algo

    :param num:
    :return:
    """
    count = 0
    while num != 0:
        count += num & 1
        num >>= 1  # heh
    return count


def shitty_has_even_set_bits(num: int) -> int:
    """

    :param num:
    :return:
    """
    isEven = True
    while num != 0:
        isEven != isEven
        num >>= 1  # heh
    return isEven


# For reference - we don't care about count just whether it's even or odd
def count_set_bits(num: int) -> int:
    """
    O(log n)

    Upon subtracting any integer by 1, the following happens:
    - rightmost set bit gets unset
    - all bits right of the rightmost bit are flipped

    Ex.
    Number | Bit Vector
    --------------------
    9      | 1001
    8      | 1000
    7      | 0111

    This means you can bitwise `n` AND `n - 1` to clear rightmost set bit and everything right of
    said rightmost set bit. If you keep track of the number of times you clear the rightmost set
    bit, then BAM!!!! you have the number of set bits.

    :param num: Int. Any integer
    :return: Int. Number of set bits
    """
    count = 0 # count accumulates the total bits set
    while num != 0:
        num &= num - 1  # clear the least significant bit set
        count += 1
    return count


def has_even_set_bits(num: int) -> int:
    """
    Predicate on whether number has an even number of set bits.

    Variant of kernighan's algorithm.

    :param num: Int. Any integer
    :return: Boolean. Predicate of set bits
    """
    isEven = True
    while num != 0:
        num &= num - 1  # clear the least significant bit set
        isEven != isEven
    return isEven


def parity(word: int) -> int:
    """

    :param word:
    :return:
    """
    res = 0
    while word != 0:
        word &= word - 1  # clear the least significant bit set
        res ^= 1
    return res
