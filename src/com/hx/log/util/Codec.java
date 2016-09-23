/**
 * file name : Codec.java
 * created at : 下午3:21:31 2016年9月6日
 * created by 970655147
 */

package com.hx.log.util;

import java.security.MessageDigest;
import java.security.SecureRandom;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.DESKeySpec;
import javax.crypto.spec.SecretKeySpec;

// 这两个还必须显式的写出来才行啊..
// import sun.misc.BASE64Encoder;
// import sun.misc.BASE64Decoder;
import sun.misc.BASE64Encoder;
import sun.misc.BASE64Decoder;

// 编码解码工具
public final class Codec {
	
	// disable constructor
	private Codec() {
		Tools.assert0("can't instantiate !");
	}
	
	// 十六进制的字符, 默认的返回字符串[编码的过程中发生了Exception]
	public static final char hexDigits[]={'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'};    
	public static final byte[] DEFAULT_RESULT = null;
	
	/**
	 * @Name: md5 
	 * @Description: 对于给定的字符串进行md5加密
	 * @param str
	 * @return  
	 * @Create at 2016年9月6日 下午3:28:36 by '970655147'
	 */
	public final static byte[] md5(byte[] inputBytes) {
		try {
			String alogrithm = "MD5";
			MessageDigest digest = MessageDigest.getInstance(alogrithm);
			digest.update(inputBytes);
			byte[] res = digest.digest();
			
			return res;
       } catch (Exception e) {
    	   e.printStackTrace();
       }

    	return DEFAULT_RESULT;
	}
	
	/**
	 * @Name: des 
	 * @Description: 对于给定的源字节序列使用key进行des 加密 / 解密
	 * @param str
	 * @param key
	 * @param encoding 加密 / 解密
	 * @return  
	 * @Create at 2016年9月6日 下午3:29:44 by '970655147'
	 */
	public static byte[] des(byte[] str, byte[] key, boolean encoding) { 
		try{
			String alogrithm = "DES";
			SecureRandom random = new SecureRandom();
			DESKeySpec desKey = new DESKeySpec(key);
			SecretKeyFactory keyFactory = SecretKeyFactory.getInstance(alogrithm);
			SecretKey securekey = keyFactory.generateSecret(desKey);
			Cipher cipher = Cipher.getInstance(alogrithm);
			
			if(encoding) {
				cipher.init(Cipher.ENCRYPT_MODE, securekey, random);
			} else {
				cipher.init(Cipher.DECRYPT_MODE, securekey, random);
			}
			
			return cipher.doFinal(str);
		}catch(Exception e){
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
	 * @Name: tripleDes 
	 * @Description: 对于给定的源字节序列使用key进行3des 加密 / 解密
	 * @param inputBytes
	 * @param key
	 * @param encoding 加密 / 解密
	 * @return  
	 * @Create at 2016年9月6日 下午3:46:35 by '970655147'
	 */
    public static byte[] tripleDes(byte[] inputBytes, byte[] key, boolean encoding){  
        try {  
        	String alogrithm = "DESede";
        	SecretKey deskey = new SecretKeySpec(key, alogrithm);  
        	Cipher cipher = Cipher.getInstance(alogrithm);
        	
        	if(encoding) {
	        	cipher.init(Cipher.ENCRYPT_MODE, deskey);  
        	} else {
        		cipher.init(Cipher.DECRYPT_MODE, deskey);  
        	}
        	
        	return cipher.doFinal(inputBytes);
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
     * @Name: base64 
     * @Description: 对给定的字节序列进行base64加密 / 解密
     * @param inputBytes
     * @param encoding 加密 / 解密
     * @return  
     * @Create at 2016年9月6日 下午3:54:55 by '970655147'
     */
    public static byte[] base64(byte[] inputBytes, String charset, boolean encoding) {
    	try {
	    	if(encoding) {
	    		return BASE64_ENCODER.encode(inputBytes).getBytes(charset);
	    	} else {
	    		return BASE64_DECODER.decodeBuffer(new String(inputBytes, charset) );
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
	 * @Name: byte2Hex 
	 * @Description: TODO
	 * @param bytes
	 * @return  
	 * @Create at 2016年9月6日 下午3:38:31 by '970655147'
	 */
	public static String byte2Hex(byte[] bytes) {
        char chars[] = new char[bytes.length << 1];
        int idx = 0;
        for (int i = 0; i < bytes.length; i++) {
            byte theByte = bytes[i];
            chars[idx++] = hexDigits[(theByte >>> 4) & 0xf];
            chars[idx++] = hexDigits[theByte & 0xf];
        }
        
        return new String(chars);
	}
	
	

}
