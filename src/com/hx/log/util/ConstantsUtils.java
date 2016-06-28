/**
 * file name : ConstantsUtils.java
 * created at : 下午4:59:23 2016年6月25日
 * created by 970655147
 */

package com.hx.log.util;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

public class ConstantsUtils {
	
	// 相关常量
	public static String sep = "=";
	public static String props = "props";
	
	// 标记各个type
	public static final int withStaticFields = 0;
	public static final int withOpt = withStaticFields + 1;
	
	/**
	 * @Name: generateConstants 
	 * @Description: 根据给定的相关配置文件中的信息, 生成应该属于Constants中的内容[我这里 仅仅以我的偏好来写]
	 * @param configPath
	 * @return
	 * @throws IOException  
	 * @Create at 2016年6月25日 下午5:32:49 by '970655147'
	 */
	public static String generateCodes0(String configPath, int typeIdx) throws Exception {
		List<String> lines = Tools.getContentWithList(configPath, 100);
		Map<String, String> prop = new LinkedHashMap<>();
		Set<Integer> appendCRLF = new HashSet<>();
		
		for(int i=0; i<lines.size(); i++) {
			String line = lines.get(i);
			if(isEmptyOrComment(line) ) {
				continue ;
			}
			
			String[] splits = line.split(sep);
			Tools.assert0(splits.length >= 2, "not good format : " + line);
			prop.put(splits[0].trim(), splits[1].trim() );
			if(isEmptyOrComment(lines.get(i+1)) ) {
				appendCRLF.add(prop.size() );
			}
		}
		
		String codes = null;
		if(typeIdx == withStaticFields) {
			codes = generateCodesWithStaticFields(prop, appendCRLF);
		} else if(typeIdx == withOpt) {
			codes = generateCodesWithOpt(prop, appendCRLF);
		} else {
			Tools.assert0("have no type with : " + typeIdx);
		}
		
		return codes;
	}
	
	/**
	 * @Name: generateCodesWithStaticFields 
	 * @Description: 根据给定的配置文件生成对应的Constants代码
	 * @param configPath
	 * @return
	 * @throws Exception  
	 * @Create at 2016年6月28日 下午9:18:17 by '970655147'
	 */
	public static String generateCodesWithStaticFields(String configPath) throws Exception {
		return generateCodes0(configPath, withStaticFields);
	}
	public static String generateCodesWithStaticFields(Map<String, String> prop, Set<Integer> appendCRLF) {
		StringBuilder sb = new StringBuilder();
		List<String> types = new ArrayList<>(prop.size() );

		int idx = 0;
		// public static final String defaultXXX = xx
		for(Entry<String, String> entry : prop.entrySet() ) {
			String type = confirmType(entry.getValue() );
			types.add(type);
			if(STRING == type) {
				Tools.appendCRLF(sb, "public static final " + type + " " + getDefaultAttrName(entry.getKey()) + " = \"" + entry.getValue() + "\";" );
			} else {
				Tools.appendCRLF(sb, "public static final " + type + " " + getDefaultAttrName(entry.getKey()) + " = " + entry.getValue() + ";" );
			}

			idx ++;
			if(appendCRLF.contains(idx) ) {
				Tools.appendCRLF(sb, Tools.EMPTY_STR);
			}
		}
		Tools.appendCRLF(sb, Tools.EMPTY_STR);
		
		idx = 0;
		// static String XXX = xx
		for(Entry<String, String> entry : prop.entrySet() ) {
			String type = confirmType(entry.getValue() );
			Tools.appendCRLF(sb, "static " + type + " " + getAttrName(entry.getKey()) + " = " + getDefaultAttrName(entry.getKey()) + ";" );

			idx ++;
			if(appendCRLF.contains(idx) ) {
				Tools.appendCRLF(sb, Tools.EMPTY_STR);
			}
		}
		Tools.appendCRLF(sb, Tools.EMPTY_STR);
		
		idx = 0;
		// XXX = props.getProperty('key', xx);
		Tools.appendCRLF(sb, "public static void load(Map<String, String> props) { ");
		for(Entry<String, String> entry : prop.entrySet() ) {
			String type = confirmType(entry.getValue() );
			if(types.get(idx) == STRING) {
				Tools.appendCRLF(sb, "	" + getAttrName(entry.getKey()) + " = Tools.optString(" + props + ", \"" + entry.getKey() + "\", " + getDefaultAttrName(entry.getKey()) + ");");
			} else {
				if(INTEGER == types.get(idx) ) {
					Tools.appendCRLF(sb, "	" + getAttrName(entry.getKey()) + " = Tools.optInt(" + props + ", \"" + entry.getKey() + "\", " + getDefaultAttrName(entry.getKey()) + ");");
				} else if(BOOLEAN == types.get(idx) ) {
					Tools.appendCRLF(sb, "	" + getAttrName(entry.getKey()) + " = Tools.optBoolean(" + props + ", \"" + entry.getKey() + "\", " + getDefaultAttrName(entry.getKey()) + ");");
				} else if(FLOAT == types.get(idx) ) {
					Tools.appendCRLF(sb, "	" + getAttrName(entry.getKey()) + " = Tools.optFloat(" + props + ", \"" + entry.getKey() + "\", " + getDefaultAttrName(entry.getKey()) + ");");
				}
			}

			idx ++;
			if(appendCRLF.contains(idx) ) {
				Tools.appendCRLF(sb, Tools.EMPTY_STR);
			}
		}
		Tools.appendCRLF(sb, "}");
		Tools.appendCRLF(sb, Tools.EMPTY_STR);
		
		return sb.toString();
	}
	
	/**
	 * @Name: generateCodesWithOpt 
	 * @Description: 根据给定的配置文件生成对应的Constants代码
	 * @param config
	 * @param appendCRLF
	 * @return  
	 * @throws Exception 
	 * @Create at 2016年6月28日 下午9:18:41 by '970655147'
	 */
	public static String generateCodesWithOpt(String configPath) throws Exception {
		return generateCodes0(configPath, withOpt);
	}
	public static String generateCodesWithOpt(Map<String, String> config, Set<Integer> appendCRLF) {
	    StringBuilder sb = new StringBuilder();

	    int idx = 0;
	    Tools.appendCRLF(sb, "// 配置相关常量 ");
	    for(Map.Entry<String, String> entry : config.entrySet() ){
	        Tools.appendCRLF(sb, "public static final String " + entry.getKey() + " = \"" + entry.getKey() + "\";" );

	        idx ++;
	        if(appendCRLF.contains(idx) ) {
	            Tools.appendCRLF(sb, Tools.EMPTY_STR);
	        }
	    }

	    Tools.appendCRLF(sb, Tools.EMPTY_STR);
	    Tools.appendCRLF(sb, "// 默认的配置 ");
	    Tools.appendCRLF(sb, "public static final Map<String, String> defaultProps = new HashMap<>(); ");
	    Tools.appendCRLF(sb, "static {");
	    idx = 0;
	    for(Map.Entry<String, String> entry : config.entrySet() ) {
	        // 一定要替换\r\n, 不能仅仅使用\n
	        String val = entry.getValue().replace("\r\n", "\\r\\n");
	        Tools.appendCRLF(sb, "	defaultProps.put(" + entry.getKey() + ", \"" + val + "\"); ");

	        idx ++;
	        if(appendCRLF.contains(idx) ) {
	            Tools.appendCRLF(sb, Tools.EMPTY_STR);
	        }
	    }
	    Tools.appendCRLF(sb, "}");

	    Tools.appendCRLF(sb, Tools.EMPTY_STR);
	    Tools.appendCRLF(sb, "// 获取相关默认值");
	    Tools.appendCRLF(sb, "" +
                "public static String optString(String key, String defaultVal) {\n" +
                "String val = (props != null) ? props.get(key) : null;\n" +
                "\tval = (val != null) ? val : defaultProp.get(key);\n" +
                "\n" +
                "\tif(val == null) {\n" +
                "\t\treturn defaultVal;\n" +
                "\t}\n" +
                "\treturn val;\n" +
                "}\n" +
                "public static int optInt(String key, int defaultVal) {\n" +
                "\tString val = optString(key);\n" +
                "\n" +
                "\tif(isEmpty0(val) ) {\n" +
                "\t\treturn defaultVal;\n" +
                "\t}\n" +
                "\treturn Integer.parseInt(val);\n" +
                "}\n" +
                "public static long optLong(String key, long defaultVal) {\n" +
                "\tString val = optString(key);\n" +
                "\n" +
                "\tif(isEmpty0(val) ) {\n" +
                "\t\treturn defaultVal;\n" +
                "\t}\n" +
                "\treturn Long.parseLong(val);\n" +
                "}\n" +
                "public static boolean optBoolean(String key, boolean defaultVal) {\n" +
                "\tString val = optString(key);\n" +
                "\n" +
                "\tif(isEmpty0(val) ) {\n" +
                "\t\treturn defaultVal;\n" +
                "\t}\n" +
                "\treturn Boolean.parseBoolean(val);\n" +
                "}\n" +
                "public static double optDouble(String key, double defaultVal) {\n" +
                "\tString val = optString(key);\n" +
                "\n" +
                "\tif(isEmpty0(val) ) {\n" +
                "\t\treturn defaultVal;\n" +
                "\t}\n" +
                "\treturn Double.parseDouble(val);\n" +
                "}\n" +
                "\n" +
                "public static String optString(String key) {\n" +
                "\treturn optString(key, null);\n" +
                "}\n" +
                "public static int optInt(String key) {\n" +
                "\treturn optInt(key, 0);\n" +
                "}\n" +
                "public static long optLong(String key) {\n" +
                "\treturn optLong(key, 0l);\n" +
                "}\n" +
                "public static boolean optBoolean(String key) {\n" +
                "\treturn optBoolean(key, false);\n" +
                "}\n" +
                "public static double optDouble(String key) {\n" +
                "\treturn optDouble(key, 0.0d);\n" +
                "}");
	    
	    return sb.toString();
	}

	// 相关类型
	public static final String INTEGER = "int";
	public static final String FLOAT = "float";
	public static final String BOOLEAN = "boolean";
	public static final String STRING = "String";
	
	/**
	 * @Name: confirmType 
	 * @Description: 根据给定的值, 确定给定的值的类型[估计]
	 * @param value
	 * @return  
	 * @Create at 2016年6月25日 下午5:29:08 by '970655147'
	 */
	private static String confirmType(String value) {
		String type = STRING;
		if(type == STRING) {
			try {
				 Integer.parseInt(value);
				 type = INTEGER;
			} catch (Exception e) {
				
			}
		}
		if(type == STRING) {
			if(Tools.equalsIgnoreCase(Constants.TRUE, value) || Tools.equalsIgnoreCase(Constants.FALSE, value) ) {
				type = BOOLEAN;
			}
		}
		if(type == STRING) {
			try {
				Float.parseFloat(value);
				type = FLOAT;
			} catch (Exception e) {
				
			}
		}
		
		return type;
	}

	/**
	 * @Name: isComment 
	 * @Description: 判断给定的行是否为注释行
	 * @param line
	 * @return  
	 * @Create at 2016年6月25日 下午5:50:05 by '970655147'
	 */
	private static boolean isEmptyOrComment(String line) {
		String trimmed = line.trim();
		if(Tools.isEmpty(trimmed) ) {
			return true;
		}
		if(trimmed.startsWith("#") || trimmed.startsWith("//") || trimmed.startsWith("rem") || trimmed.startsWith(";") ) {
			return true;
		}
		
		return false;
	}
	
	/**
	 * @Name: getAttrName 
	 * @Description: 获取给定的属性的名称
	 * @param key
	 * @return  
	 * @Create at 2016年6月25日 下午6:08:44 by '970655147'
	 */
	private static String getAttrName(String key) {
		return Tools.camel2UnderLine(key).toUpperCase();
	}
	/**
	 * @Name: getDefaultAttrName 
	 * @Description: 获取给定的属性的默认属性名
	 * @param key
	 * @return  
	 * @Create at 2016年6月25日 下午6:05:49 by '970655147'
	 */
	private static String getDefaultAttrName(String key) {
		return "DEFAULT_" + Tools.camel2UnderLine(key).toUpperCase();
	}

}
