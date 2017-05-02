/**
 * file name : Codec.java
 * created at : ����3:21:31 2016��9��6��
 * created by 970655147
 */

package com.hx.log.alogrithm.code;

import com.hx.log.util.Tools;
import sun.misc.BASE64Decoder;
import sun.misc.BASE64Encoder;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.DESKeySpec;
import javax.crypto.spec.SecretKeySpec;
import java.security.MessageDigest;
import java.security.SecureRandom;

// ��������������ʽ��д�������а�..
// import sun.misc.BASE64Encoder;
// import sun.misc.BASE64Decoder;

// ������빤��
public final class Codec {

    // disable constructor
    private Codec() {
        Tools.assert0("can't instantiate !");
    }

    // ʮ�����Ƶ��ַ�, Ĭ�ϵķ����ַ���[����Ĺ����з�����Exception]
    public static final char HEX_DIGITS[] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};
    public static final byte[] DEFAULT_RESULT = null;

    /**
     * @param bytes ������ֽ�����
     * @return
     * @Name: md5
     * @Description: ���ڸ������ַ�������md5����
     * @Create at 2016��9��6�� ����3:28:36 by '970655147'
     */
    public static byte[] md5(byte[] bytes) {
        Tools.assert0(bytes != null, "'bytes' can't be null !");
        try {
            String alogrithm = "MD5";
            MessageDigest digest = MessageDigest.getInstance(alogrithm);
            digest.update(bytes);
            byte[] res = digest.digest();

            return res;
        } catch (Exception e) {
            e.printStackTrace();
        }

        return DEFAULT_RESULT;
    }

    /**
     * @param bytes      ����ǰ, ���ܺ���ֽ�����ϵ��
     * @param key      ����/���� �õ�key
     * @param encoding ���� / ����
     * @return
     * @Name: des
     * @Description: ���ڸ�����Դ�ֽ�����ʹ��key����des ���� / ����
     * @Create at 2016��9��6�� ����3:29:44 by '970655147'
     */
    public static byte[] des(byte[] bytes, byte[] key, boolean encoding) {
        Tools.assert0(bytes != null, "'bytes' can't be null !");
        Tools.assert0(key != null, "'key' can't be null !");

        try {
            String alogrithm = "DES";
            SecureRandom random = new SecureRandom();
            DESKeySpec desKey = new DESKeySpec(key);
            SecretKeyFactory keyFactory = SecretKeyFactory.getInstance(alogrithm);
            SecretKey securekey = keyFactory.generateSecret(desKey);
            Cipher cipher = Cipher.getInstance(alogrithm);

            if (encoding) {
                cipher.init(Cipher.ENCRYPT_MODE, securekey, random);
            } else {
                cipher.init(Cipher.DECRYPT_MODE, securekey, random);
            }

            return cipher.doFinal(bytes);
        } catch (Exception e) {
            e.printStackTrace();
        }

        return DEFAULT_RESULT;
    }

    public static byte[] desE(byte[] str, byte[] key) {
        return des(str, key, true);
    }

    public static byte[] desD(byte[] str, byte[] key) {
        return des(str, key, false);
    }

    /**
     * @param bytes    ������ֽ�����
     * @param key      ����/ �����õ�key
     * @param encoding ���� / ����
     * @return
     * @Name: tripleDes
     * @Description: ���ڸ�����Դ�ֽ�����ʹ��key����3des ���� / ����
     * @Create at 2016��9��6�� ����3:46:35 by '970655147'
     */
    public static byte[] tripleDes(byte[] bytes, byte[] key, boolean encoding) {
        Tools.assert0(bytes != null, "'bytes' can't be null !");
        Tools.assert0(key != null, "'key' can't be null !");

        try {
            String alogrithm = "DESede";
            SecretKey deskey = new SecretKeySpec(key, alogrithm);
            Cipher cipher = Cipher.getInstance(alogrithm);

            if (encoding) {
                cipher.init(Cipher.ENCRYPT_MODE, deskey);
            } else {
                cipher.init(Cipher.DECRYPT_MODE, deskey);
            }

            return cipher.doFinal(bytes);
        } catch (Exception e) {
            e.printStackTrace();
        }

        return DEFAULT_RESULT;
    }

    public static byte[] tripleDesE(byte[] inputBytes, byte[] key) {
        return tripleDes(inputBytes, key, true);
    }

    public static byte[] tripleDesD(byte[] inputBytes, byte[] key) {
        return tripleDes(inputBytes, key, false);
    }

    public static byte[] _3Des(byte[] inputBytes, byte[] key, boolean encoding) {
        return tripleDes(inputBytes, key, encoding);
    }

    public static byte[] _3DesE(byte[] inputBytes, byte[] key) {
        return tripleDes(inputBytes, key, true);
    }

    public static byte[] _3DesD(byte[] inputBytes, byte[] key) {
        return tripleDes(inputBytes, key, false);
    }

    // encoder, decoder
    public static final BASE64Encoder BASE64_ENCODER = new BASE64Encoder();
    public static final BASE64Decoder BASE64_DECODER = new BASE64Decoder();

    /**
     * @param bytes    ����ǰ/����ǰ���ֽ�����
     * @param encoding ���� / ����
     * @return
     * @Name: base64
     * @Description: �Ը������ֽ����н���base64���� / ����
     * @Create at 2016��9��6�� ����3:54:55 by '970655147'
     */
    public static byte[] base64(byte[] bytes, String charset, boolean encoding) {
        Tools.assert0(bytes != null, "'bytes' can't be null !");
        Tools.assert0(charset != null, "'charset' can't be null !");

        try {
            if (encoding) {
                return BASE64_ENCODER.encode(bytes).getBytes(charset);
            } else {
                return BASE64_DECODER.decodeBuffer(new String(bytes, charset));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return DEFAULT_RESULT;
    }

    public static byte[] base64E(byte[] inputBytes, String charset) {
        return base64(inputBytes, charset, true);
    }

    public static byte[] base64D(byte[] inputBytes, String charset) {
        return base64(inputBytes, charset, false);
    }

    public static byte[] base64E(byte[] inputBytes) {
        return base64E(inputBytes, Tools.ISO_8859_1);
    }

    public static byte[] base64D(byte[] inputBytes) {
        return base64D(inputBytes, Tools.ISO_8859_1);
    }

    /**
     * @param bytes ������ֽ�����
     * @return
     * @Name: byte2Hex
     * @Description: ���������ֽ�����, һ���ֽڲ�ֳ�����ʮ�������ַ�, ���ؽ������ֽڲ��֮����ַ���
     * @Create at 2016��9��6�� ����3:38:31 by '970655147'
     */
    public static String byte2Hex(byte[] bytes) {
        Tools.assert0(bytes != null, "'charset' can't be null !");

        char chars[] = new char[bytes.length << 1];
        int idx = 0;
        for (int i = 0; i < bytes.length; i++) {
            byte theByte = bytes[i];
            chars[idx++] = HEX_DIGITS[(theByte >>> 4) & 0xf];
            chars[idx++] = HEX_DIGITS[theByte & 0xf];
        }

        return new String(chars);
    }


}
