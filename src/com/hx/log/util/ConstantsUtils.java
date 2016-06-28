/**
 * file name : ConstantsUtils.java
 * created at : ����4:59:23 2016��6��25��
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
	
	// ��س���
	public static String sep = "=";
	public static String props = "props";
	
	// ��Ǹ���type
	public static final int withStaticFields = 0;
	public static final int withOpt = withStaticFields + 1;
	
	/**
	 * @Name: generateConstants 
	 * @Description: ���ݸ�������������ļ��е���Ϣ, ����Ӧ������Constants�е�����[������ �������ҵ�ƫ����д]
	 * @param configPath
	 * @return
	 * @throws IOException  
	 * @Create at 2016��6��25�� ����5:32:49 by '970655147'
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
	 * @Description: ���ݸ����������ļ����ɶ�Ӧ��Constants����
	 * @param configPath
	 * @return
	 * @throws Exception  
	 * @Create at 2016��6��28�� ����9:18:17 by '970655147'
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
	 * @Description: ���ݸ����������ļ����ɶ�Ӧ��Constants����
	 * @param config
	 * @param appendCRLF
	 * @return  
	 * @throws Exception 
	 * @Create at 2016��6��28�� ����9:18:41 by '970655147'
	 */
	public static String generateCodesWithOpt(String configPath) throws Exception {
		return generateCodes0(configPath, withOpt);
	}
	public static String generateCodesWithOpt(Map<String, String> config, Set<Integer> appendCRLF) {
	    StringBuilder sb = new StringBuilder();

	    int idx = 0;
	    Tools.appendCRLF(sb, "// ������س��� ");
	    for(Map.Entry<String, String> entry : config.entrySet() ){
	        Tools.appendCRLF(sb, "public static final String " + entry.getKey() + " = \"" + entry.getKey() + "\";" );

	        idx ++;
	        if(appendCRLF.contains(idx) ) {
	            Tools.appendCRLF(sb, Tools.EMPTY_STR);
	        }
	    }

	    Tools.appendCRLF(sb, Tools.EMPTY_STR);
	    Tools.appendCRLF(sb, "// Ĭ�ϵ����� ");
	    Tools.appendCRLF(sb, "public static final Map<String, String> defaultProps = new HashMap<>(); ");
	    Tools.appendCRLF(sb, "static {");
	    idx = 0;
	    for(Map.Entry<String, String> entry : config.entrySet() ) {
	        // һ��Ҫ�滻\r\n, ���ܽ���ʹ��\n
	        String val = entry.getValue().replace("\r\n", "\\r\\n");
	        Tools.appendCRLF(sb, "	defaultProps.put(" + entry.getKey() + ", \"" + val + "\"); ");

	        idx ++;
	        if(appendCRLF.contains(idx) ) {
	            Tools.appendCRLF(sb, Tools.EMPTY_STR);
	        }
	    }
	    Tools.appendCRLF(sb, "}");

	    Tools.appendCRLF(sb, Tools.EMPTY_STR);
	    Tools.appendCRLF(sb, "// ��ȡ���Ĭ��ֵ");
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

	// �������
	public static final String INTEGER = "int";
	public static final String FLOAT = "float";
	public static final String BOOLEAN = "boolean";
	public static final String STRING = "String";
	
	/**
	 * @Name: confirmType 
	 * @Description: ���ݸ�����ֵ, ȷ��������ֵ������[����]
	 * @param value
	 * @return  
	 * @Create at 2016��6��25�� ����5:29:08 by '970655147'
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
	 * @Description: �жϸ��������Ƿ�Ϊע����
	 * @param line
	 * @return  
	 * @Create at 2016��6��25�� ����5:50:05 by '970655147'
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
	 * @Description: ��ȡ���������Ե�����
	 * @param key
	 * @return  
	 * @Create at 2016��6��25�� ����6:08:44 by '970655147'
	 */
	private static String getAttrName(String key) {
		return Tools.camel2UnderLine(key).toUpperCase();
	}
	/**
	 * @Name: getDefaultAttrName 
	 * @Description: ��ȡ���������Ե�Ĭ��������
	 * @param key
	 * @return  
	 * @Create at 2016��6��25�� ����6:05:49 by '970655147'
	 */
	private static String getDefaultAttrName(String key) {
		return "DEFAULT_" + Tools.camel2UnderLine(key).toUpperCase();
	}

}
