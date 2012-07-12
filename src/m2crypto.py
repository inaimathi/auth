from erlport import Port, Protocol, String
import M2Crypto

# def gimme_pw(*args):
#     return "some sort of secure password goes here (please change it)"

def load_key(key, password):
    if password:
        key = M2Crypto.RSA.load_key_string(pem, callback=lambda *args: password)
    else:
        key = M2Crypto.RSA.load_key_string(pem)
    return key
    
class M2cryptoProtocol(Protocol):
    # def handle_generate(self):
    #     keyname = "server_auth.key"
    #     Cert = M2Crypto.RSA.gen_key(4096, 65537)
    #     Cert.save_key(keyname, callback=gimme_pw)
    #     return keyname
    def handle_sign(self, pem, password, message):
        Key = load_key(pem, password)
        Digest = M2Crypto.EVP.MessageDigest('sha256')
        Digest.update(message)
        return Key.sign_rsassa_pss(Digest.digest()).encode('base64')
    def handle_decrypt(self, pem, password, message):
        Key = load_key(pem, password)
        try:
            return Key.private_decrypt(message, M2Crypto.RSA.pkcs1_oaep_padding)
        except:
            return False
    def handle_split_key(self, filename):
        pubkey = M2Crypto.RSA.load_pub_key(String(filename))
        return (pubkey.e, pubkey.n)
    def handle_encrypt(self, e, n, message):
        pubkey = M2Crypto.RSA.new_pub_key((str(e), str(n)))
        ciphertext = pubkey.public_encrypt(String(message), M2Crypto.RSA.pkcs1_oaep_padding)
        return ciphertext.encode('base64')
    def handle_verify(self, e, n, message, sig):
        pubkey = M2Crypto.RSA.new_pub_key((str(e), str(n)))
        try:
            if pubkey.verify_rsassa_pss(String(message), String(sig).decode('base64')):
                return True
            else:
                return False
        except:
            return False

if __name__ == "__main__":
    M2cryptoProtocol().run(Port(packet=4, use_stdio=True))
