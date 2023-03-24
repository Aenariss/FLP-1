from ecdsa import SigningKey, SECP256k1

# TOTO JSOU MOJE PARAMETRY - SECP256-k1
P = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F
n = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
a = 0
b = 7
Gx = 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
Gy = 0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8


# Certicom secp256-k1
#_a = 0x0000000000000000000000000000000000000000000000000000000000000000
#_b = 0x0000000000000000000000000000000000000000000000000000000000000007
#_p = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F
#_Gx = 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
#_Gy = 0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8
#_r = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141


# Generate a private key
private_key_hex = 'c9dcda39c4d7ab9d854484dbed2963da9c0cf3c6e9333528b4422ef00dd0b28e'
priv_key = 0xc9dcda39c4d7ab9d854484dbed2963da9c0cf3c6e9333528b4422ef00dd0b28e
private_key = SigningKey.from_string(bytes.fromhex(private_key_hex), curve=SECP256k1)

#private key dekoduju na hexa spravne, nacitani curvy taky spravne i guess

call_count = 0

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
    global call_count
    call_count += 1
    # Calculate the modular inverse of x modulo p
    return pow(x, p-2, p)

def double_add(tmp, res=(0,0)):
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
print("Public key:", public_key.to_string().hex())

#6447434313803670400611396947767420002184834394589992108199185664706742207204
#6447434313803670400611396947767420002184834394589992108199185664706742207204