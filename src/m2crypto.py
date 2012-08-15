from erlport import Port, Protocol, String
import M2Crypto, sys

class M2cryptoProtocol(Protocol):
    def handle_encrypt(self, e, n, message):
        pubkey = M2Crypto.RSA.new_pub_key((str(e), str(n)))
        ciphertext = pubkey.public_encrypt(String(message), M2Crypto.RSA.pkcs1_oaep_padding)
        return ciphertext.encode('base64')
    def handle_verify(self, e, n, message, sig):
        pubkey = M2Crypto.RSA.new_pub_key((str(e), str(n)))
        if pubkey.verify_rsassa_pss(String(message), String(sig).decode('base64')):
            return True
        else:
            return False
    def handle_sign(self, pem, message):
        try:
            Key = M2Crypto.RSA.load_key_string(pem)
        except:
            return "key read error"
        return Key.sign_rsassa_pss(String(message)).encode('base64')
    def handle_decrypt(self, pem, message):
        try:
            Key = M2Crypto.RSA.load_key_string(pem)
        except:
            return "key read error"
        clear = Key.private_decrypt(String(message).decode('base64'), M2Crypto.RSA.pkcs1_oaep_padding)
        return clear
    def handle_split_key(self, filename):
        try:
            pubkey = M2Crypto.RSA.load_pub_key(String(filename))
            return (pubkey.e, pubkey.n)
        except:
            return False

if __name__ == "__main__":
    M2cryptoProtocol().run(Port(packet=4, use_stdio=True))
