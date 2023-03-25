from ecdsa import SigningKey, SECP256k1, util
import hashlib

# TOTO JSOU MOJE PARAMETRY - SECP256-k1
P = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F
n = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
a = 0
b = 7
Gx = 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
Gy = 0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8


# Generate a private key
private_key_hex = 'c9dcda39c4d7ab9d854484dbed2963da9c0cf3c6e9333528b4422ef00dd0b28e'
priv_key = 0xc9dcda39c4d7ab9d854484dbed2963da9c0cf3c6e9333528b4422ef00dd0b28e
private_key = SigningKey.from_string(bytes.fromhex(private_key_hex), curve=SECP256k1)

def add (p, q):


    if p == (0,0): return q
    if q == (0,0): return p

    xp, yp = p
    xq, yq = q
    if (p == q):
        if yp == 0: return (0,0)
        m = ((3*pow(xp,2)+a) * inverse_modulo(2*yp, P)) % P
    elif (xp == xq): return (0,0)
    else:
        m = ((yq-yp) * inverse_modulo((xq-xp), P)) % P
    
    xr = (m*m - (xp + xq)) % P
    yr = (m*(xp-xr)-yp) % P
    return (xr, yr)

def inverse_modulo(x, p):
    # Calculate the modular inverse of x modulo p
    return pow(x, p-2, p)

def double_add(tmp, res=(0,0)):
    global n
    bits = format(priv_key, 'b')[::-1]
    for bit in bits:
        if bit == '1':
            res = add(res, tmp)
        tmp = add(tmp, tmp)
        
    a = hex(res[0])[2:]
    b = hex(res[1])[2:]
    if len (a+b) % 2 == 1: return "0" +a+b
    else: return a+b

print("Public key:", "0x04" + double_add((Gx,Gy)))
# Get the corresponding public key
public_key = private_key.get_verifying_key()

# Print the public key in hexadecimal format
#print("Public key:", public_key.to_string().hex())
hashed_message = 0x94996fead5b722c3bd07360c459927976e804f869626f4897def03fa56b009e3.to_bytes(len("0x94996fead5b722c3bd07360c459927976e804f869626f4897def03fa56b009e3")//2-1, byteorder="big")


import ecdsa
from ecdsa.util import sigencode_der

# Specify the hash, curve, and private key
hashed_message = 0x94996fead5b722c3bd07360c459927976e804f869626f4897def03fa56b009e3.to_bytes(len("0x94996fead5b722c3bd07360c459927976e804f869626f4897def03fa56b009e3")//2-1, byteorder="big")

# Create the signature
sig_obj = private_key.sign_deterministic(hashed_message, hashfunc=hashlib.sha3_256, sigencode=ecdsa.util.sigencode_der)

# Print the signature's parts (r and s)
r, s = ecdsa.util.sigdecode_der(sig_obj, private_key.curve.order)
print(f"r: {hex(r)}\ns: {s}")