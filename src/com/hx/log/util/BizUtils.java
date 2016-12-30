/**
 * file name : BizUtils.java
 * created at : 22:51:02 2016-12-30
 * created by 970655147
 */

package com.hx.log.util;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

public final class BizUtils {
	
	// disable constructor
	private BizUtils() {
		Tools.assert0("can't instantiate !");
	}
	

	// 所有的数字的Character
	static Set<Character> nums = new HashSet<>();
	static {
		for(char i='0'; i<='9'; i++) {
			nums.add(i);
		}
	}
	
	// 处理价格, 也可以用于处理提取字符串中的BigDecimal的情况
	public static BigDecimal dealPrice(String str) {
		if(Tools.isEmpty(str)) {
			return Tools.BIGDEC_ZERO;
		}
		
		StringBuilder sb = new StringBuilder();
		for(int i=0; i<str.length(); i++) {
			char ch = str.charAt(i);
			if(nums.contains(ch) || (ch == '.') ) {
				sb.append(ch);
			}
		}
		if(sb.length() == 0) {
			return Tools.BIGDEC_ZERO;
		} else {
			return new BigDecimal(sb.toString());
		}
	}
	
	// 处理页数, 也可以用于处理提取字符串中的整数的情况
	public static Integer dealPageNum(String str) {
		if(Tools.isEmpty(str)) {
			return Tools.INTE_ZERO;
		}
		
		StringBuilder sb = new StringBuilder();
		for(int i=0; i<str.length(); i++) {
			char ch = str.charAt(i);
			if(nums.contains(ch) ) {
				sb.append(ch);
			}
		}
		if(sb.length() == 0) {
			return Tools.INTE_ZERO;
		} else {
			return new Integer(sb.toString());
		}
	}
	
	
	// 匹配siteUrl的regex
	static Pattern siteUrlPattern = Pattern.compile("^(\\w{3,5}://\\w+(\\.\\w+)+?/)(.*)");
	
	// 获取站点的首页url
	// http://www.baidu.com/tieba/java/page01.jsp  =>  http://www.baidu.com/
	public static String getSiteUrl(String url) {
		Tools.assert0(! Tools.isEmpty(url), "'url' can't be null ");
		Matcher matcher = siteUrlPattern.matcher(url);
		if(matcher.matches()) {
			return matcher.group(1);
		}
		return null;
	}
	
	// 将绝对/ 相对的url转换为绝对的url
	// 转换 /path & ./path
	public static String transformUrl(String siteUrl, String relativePath) {
		Tools.assert0(! Tools.isEmpty(siteUrl), "'siteUrl' can't be null ");
		Tools.assert0(! Tools.isEmpty(relativePath), "'relativePath' can't be null ");
		
		if(relativePath.startsWith("/") ) {
			return getSiteUrl(siteUrl) + Tools.removeIfStartsWith(relativePath, "/");
		} else if (relativePath.startsWith("./") ) {
			return siteUrl.substring(0, siteUrl.lastIndexOf("/")+1 ) + Tools.removeIfStartsWith(relativePath, "./");
		} else {
			return relativePath;
		}
	}
	
	
	// 为nextStageParams添加category
	public static void addNameUrlSite(JSONObject category, JSONObject nextStageParams) {
		Tools.assert0(! Tools.isEmpty(category), "'category' can't be null ");
		nextStageParams.put(Tools.NAME, category.getString(Tools.NAME) );
		nextStageParams.put(Tools.URL, category.getString(Tools.URL) );
		nextStageParams.put(Tools.SITE, nextStageParams.getString(Tools.SITE) + "." + category.getString(Tools.NAME) );
	}
	
	// 通过产品的数目, 以及每一页显示的产品的数目, 计算页数
	public static int calcPageNums(int productNum, int numPerPage) {
		return ((productNum-1) / numPerPage) + 1;
	}
	
	
	// 从spec中获取需要的数据
	// 注意 : 必需确保spec中每一个对象为JSONObject, name为spec的数据中需要检测的值, value为spec的数据中需要获取的值, getInSpec存放获取数据的键的(key[src源对象] -> key[dst目标对象])映射
//	[											
//	...												{
//    {													...
//        "value":" #F3F07AAR#ABA",			=>			"model":"#F3F07AAR#ABA"	
//        "name":"Model"								...
//    }												}
//	...	
//	]
	public static void getNeededFrom(JSONArray spec, JSONObject product, String name, String value, Map<String, String> getInSpec) {
		if((Tools.isEmpty(spec)) || (Tools.isEmpty(product)) || Tools.isEmpty(getInSpec) ) {
			return ;
		}
		
		Iterator<?> it = spec.iterator();
		while(it.hasNext()) {
			JSONObject val = (JSONObject) it.next();
			String key = val.getString(name);
			if(getInSpec.containsKey(key) ) {
				product.put(getInSpec.get(key), val.get(value));
			}
		}
	}
	
	
	// 获取键值对类型的数据对, 添加到headers中
	public static void addHeaders(File configFile, Map<String, String> headers, String sep) throws IOException {
		Tools.assert0(configFile != null, "'configFile' can't be null ");
		Tools.assert0(headers != null, "'headers' can't be null ");
		Tools.assert0(sep != null, "'sep' can't be null ");
		
		List<String> lines = Tools.getContentWithList(configFile);
		for(String line : lines) {
			int idx = line.indexOf(sep);
			if(idx > 0) {
				headers.put(line.substring(0, idx), line.substring(idx + 1));
			}
		}
	}
	
	// 解码含有unicode字符串的字符串
	// 遍历一次字符串, 寻找出匹配"\\uxxxx"的字符串, 然后将其解码为字符[unicode -> char]
	// 对于其他的字符不作处理
	public static String unicodeDecode(String str) {
		Tools.assert0(str != null, "'str' can't be null ");
		
		StringBuilder sb = new StringBuilder(str.length() );
		for(int i=0; i<str.length(); i++) {
			char ch = str.charAt(i);
			if(ch == Tools.SLASH) {
				char nextCh = str.charAt(i + 1);
				if(nextCh == 'u') {
					boolean isUnicode = true;
					for(int j=0; j<4; j++) {
						// '+2' escape '\\u'
						if(! isHexChar(str.charAt(i + j + 2)) ) {
							isUnicode = false;
							break ;
						}
					}
					
					// 如果"\\u"之后的四个字符可以表示为十六进制的数字, 则将其解码, 并更新i, continue
					if(isUnicode) {
						char decoded = Character.valueOf((char) Integer.valueOf(str.substring(i+2, i+6), 16).intValue());
						sb.append(decoded);
						i += 5;
						continue ;
					}
				}
			} 
			
			sb.append(ch);
		}
		
		return sb.toString();
	}
	// 判断给定的字符是否可表示十六进制[0-9, a-f, A-F]
	public static boolean isHexChar(char ch) {
		return (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F');
	}
	
	
	// ------------ 进制转换相关 --------------------
	// 根据数字和单位获取字符串表示的接口
	public static interface GetLengthStrMethod {
		public String getLengthStr(long length, String dimen);
	}
	public static final GetLengthStrMethod defaultGetLengthStrMethod = new GetLengthStrMethod() {
		public String getLengthStr(long length, String dimen) {
			return length + " " + dimen;
		}
	};
	
    // 根据长度, 获取长度的字符串表示
	public static String getLengthString(long length, String dimen) {
		return getLengthString(length, dimen, defaultGetLengthStrMethod);
	}
	public static String getLengthString(long length, String dimen, GetLengthStrMethod getLengthStrMethod) {
		long transfered = -1;
		if(Tools.equalsIgnoreCase(Tools.BYTE, dimen)) {
			transfered = length;
		} else if(Tools.equalsIgnoreCase(Tools.KB, dimen) ) {
			transfered = Tools.getKBytesByBytes(length);
		} else if(Tools.equalsIgnoreCase(Tools.MB, dimen) ) {
			transfered = Tools.getMBytesByBytes(length);
		} else if(Tools.equalsIgnoreCase(Tools.GB, dimen) ) {
			transfered = Tools.getGBytesByBytes(length);
		} else if(Tools.equalsIgnoreCase(Tools.TB, dimen) ) {
			transfered = Tools.getTBytesByBytes(length);
		} else if(Tools.equalsIgnoreCase(Tools.PB, dimen) ) {
			transfered = Tools.getPBytesByBytes(length);
		} else if(Tools.equalsIgnoreCase(Tools.EB, dimen) ) {
			transfered = Tools.getEBytesByBytes(length);
		} else if(Tools.equalsIgnoreCase(Tools.ZB, dimen) ) {
			transfered = Tools.getZBytesByBytes(length);
		} else if(Tools.equalsIgnoreCase(Tools.YB, dimen) ) {
			transfered = Tools.getYBytesByBytes(length) ;
		} else {
			Tools.assert0("unSupported Unit : " + dimen + " !");
		}
		
		return getLengthStrMethod.getLengthStr(transfered, dimen);
	}
	
	// 根据字节数, 获取千字节数, 兆字节数, 吉字节数, 踢字节数
	public static long getKBytesByBytes(long bytes) {
		return bytes >> 10;
	}
	public static long getMBytesByBytes(long bytes) {
		return bytes >> 20;
	}
	public static long getGBytesByBytes(long bytes) {
		return bytes >> 30;
	}
	public static long getTBytesByBytes(long bytes) {
		return bytes >> 40;
	}
	public static long getPBytesByBytes(long bytes) {
		return bytes >> 50;
	}
	public static long getEBytesByBytes(long bytes) {
		return bytes >> 60;
	}
	public static long getZBytesByBytes(long bytes) {
		return bytes >> 70;
	}
	public static long getYBytesByBytes(long bytes) {
		return bytes >> 80;
	}
	
	
    // add at 2016.05.17
    // 查询字符串的分隔符
    static String PARAM_KV_SEP = "=";
    static String PARAM_PARAM_SEP = "&";
    // 增加封装get请求的查询字符串
    public static String encapQueryString(Map<String, String> params) {
	   return encapQueryString0(params, PARAM_KV_SEP, PARAM_PARAM_SEP);
    }
	// cookie相关分隔符
	public static String COOKIE_KV_SEP = "=";
	public static String COOKIE_COOKIE_SEP = ";";
	// 通过cookies获取cookie的字符串表示
	public static String getCookieStr(Map<String, String> cookies) {
		return encapQueryString0(cookies, COOKIE_KV_SEP, COOKIE_COOKIE_SEP);
	}
	// 通过cookie格式的字符串 获取各个cookie [这里 直接使用split, 避免出现错误]
	public static Map<String, String> getCookiesByCookieStr(String cookiesStr) {
		String[] cookies = cookiesStr.split(COOKIE_COOKIE_SEP);
		Map<String, String> res = new HashMap<>(cookies.length );
		for(int i=0; i<cookies.length; i++) {
			String[] kvPair = cookies[i].split(COOKIE_KV_SEP);
			Tools.assert0(kvPair.length > 1, "error cookieString : '" + cookiesStr + "', around : '" + cookies[i] + "'");
			res.put(kvPair[0], kvPair[1] );
		}
		
		return res;
	}
	private static String encapQueryString0(Map<String, String> params, String KVSep, String paramsSep) {
		Tools.assert0(params != null, "'params' can't be null ");
		Tools.assert0(KVSep != null, "'KVSep' can't be null ");
		Tools.assert0(paramsSep != null, "'paramsSep' can't be null ");
		
		StringBuilder sb = new StringBuilder();
		for(Entry<String, String> entry : params.entrySet() ) {
			sb.append(entry.getKey() );	sb.append(KVSep);
			sb.append(entry.getValue());	sb.append(paramsSep);
		}
		Tools.removeLastSep(sb, paramsSep);
		
		return sb.toString();
	}
	
	
	/** 
	 * @Description: 获取给定的异常的错误信息
	 * @param e
	 * @return  
	 * @Create at 2016-12-30 23:03:08 by '970655147'
	 */
	public static String errorMsg(Exception e) {
		Tools.assert0(e != null, "'e' can't be null ");
		return e.getClass().getName() + " -> " + e.getMessage();
	}
	
	
	// add at 2016.06.18
	// 驼峰 -> 下划线表示
	private static Character underLine = '_';
	public static String camel2UnderLine(String name) {
		Tools.assert0(name != null, "'name' can't be null ");
		
		StringBuilder sb = new StringBuilder(name.length() + 10);
		boolean isLastCharUpper = Character.isUpperCase(name.charAt(0) );
//		boolean isLastCharUpper = false;
		
		for(int i=0; i<name.length(); i++) {
			char ch = name.charAt(i);
			if(Character.isUpperCase(ch) ) {
				if(! isLastCharUpper) {
					sb.append(underLine);
				}
				sb.append(Character.toLowerCase(ch) );
				isLastCharUpper = true;
			} else {
				sb.append(ch);
				isLastCharUpper = false;
			}
		}
		
		return sb.toString();
	}
	public static String underLine2Camel(String name) {
		Tools.assert0(name != null, "'name' can't be null ");
		
		StringBuilder sb = new StringBuilder(name.length() + 10);
		for(int i=0; i<name.length(); i++) {
			char ch = name.charAt(i);
			if(underLine.equals(ch) ) {
				char nextCh = name.charAt(i+1);
				if(Character.isLowerCase(nextCh) || Character.isUpperCase(nextCh) ) {
					sb.append(Character.toUpperCase(name.charAt(i+1)) );
					
					// skip '_', the end of loop skip 'nextCh'
					i ++;
				} else {
					sb.append(ch);
				}
			} else {
				sb.append(ch);
			}
		}
		
		return sb.toString();
	}
	
	// add at 2016.08.25
	public static String getClazzNameByFullName(String fullName) {
		String name = fullName;
		int lastIdxOfDot = fullName.lastIndexOf(".");
		if(lastIdxOfDot >= 0) {
			name = fullName.substring(lastIdxOfDot+1);
		}
		
		return name;
	}
	
}
