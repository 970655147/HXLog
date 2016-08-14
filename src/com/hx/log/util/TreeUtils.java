/**
 * file name : TreePattern.java
 * created at : ����8:29:35 2016��8��11��
 * created by 970655147
 */

package com.hx.log.util;

import java.util.ArrayList;
import java.util.List;

import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

public class TreeUtils {

	// add at 2016.07.06, ��ӡ����������״�ӿ�[�����ΪҶ�ڵ�, ���һ���ӽڵ�Ϊ��ǰ����Ԫ������Ϣ]
	// tree ��س���
	public static final String TREE_NAME = "name";
	public static final String TREE_TYPE = "type";
	public static final String TREE_SIZE = "size";
	
	public static final String TREE_OBJ = "obj";
	public static final String TREE_ARR = "arr";
	public static final String TREE_NULL = "NULL";
	
	// tree ���������
	private static final List<Integer> TREE_LIST_DUMMY = new ArrayList<>();
	public static int TREE_OFFSET = 0;
	public static int TREE_LENGTH_PER_SEP = 2;
	public static boolean TREE_IS_APPEND_CRLF_WHILE_NO_FILE = true;
	public static String TREE_SEPS = "-";
	public static String TREE_VERTICAL_LINE = "|";
	
	// ��ȡTreeObj, TreeArr����Ϣ�ӿ�
	public static interface TreeObjInfoExtractor {
		public String getTreeObjInfo(JSONObject obj);
	}
	public static interface TreeArrInfoExtractor {
		public String getTreeArrInfo(JSONObject obj);
	}
	public static final TreeObjInfoExtractor DEFAULT_TREE_OBJ_INFO_EXTRACTOR = new TreeObjInfoExtractor() {
		public String getTreeObjInfo(JSONObject obj) {
			return obj.getString(TREE_NAME) + "[" + Tools.getLengthString(obj.getLong(TREE_SIZE), Tools.KB) + "]";
		}
	};
	public static final TreeArrInfoExtractor DEFAULT_TREE_ARR_INFO_EXTRACTOR = new TreeArrInfoExtractor() {
		public String getTreeArrInfo(JSONObject obj) {
			return obj.getString(TREE_NAME) + "[dir]";
		}
	};
	
	// ��ӡ��������JSONArray, �����ڸ�����StringBuilder��
	public static String tree(JSONArray arr) {
		return tree(arr, DEFAULT_TREE_OBJ_INFO_EXTRACTOR, DEFAULT_TREE_ARR_INFO_EXTRACTOR);
	}
	public static String tree(JSONArray arr, TreeObjInfoExtractor objInfoExtractor, TreeArrInfoExtractor arrInfoExtractor) {
		StringBuilder sb = new StringBuilder();
		tree(arr, sb, TREE_OFFSET, TREE_LENGTH_PER_SEP, TREE_LIST_DUMMY, TREE_IS_APPEND_CRLF_WHILE_NO_FILE, objInfoExtractor, arrInfoExtractor);
		return sb.toString();
	}
	public static String tree(JSONArray arr, int offset, int lengthPerSep, boolean isAppendCRLFWhileNoFile) {
		StringBuilder sb = new StringBuilder();
		tree(arr, sb, offset, lengthPerSep, TREE_LIST_DUMMY, isAppendCRLFWhileNoFile, DEFAULT_TREE_OBJ_INFO_EXTRACTOR, DEFAULT_TREE_ARR_INFO_EXTRACTOR);
		return sb.toString();
	}
	// Լ��arr�ĵ�һ��Ԫ��Ϊ��ǰarr��Ԫ�ؾ�����
	public static int tree(JSONArray arr, StringBuilder sb, int offset, int lengthPerSep, List<Integer> verticalLines, boolean isAppendCRLFWhileNoFile, TreeObjInfoExtractor objInfoExtractor, TreeArrInfoExtractor arrInfoExtractor) {
		Tools.assert0(arr != null, "'arr' can't be null ");
		Tools.assert0(sb != null, "'sb' can't be null ");
		Tools.assert0(objInfoExtractor != null, "'objInfoExtractor' can't be null ");
		Tools.assert0(arrInfoExtractor != null, "'arrInfoExtractor' can't be null ");
		
		JSONObject meta = arr.getJSONObject(0);
		int rows = 0;
		int appendedRows = 0;

		if (Tools.equalsIgnoreCase(meta.getString(TREE_TYPE), TREE_ARR)) {
			String folerInfo = arrInfoExtractor.getTreeArrInfo(meta);
			sb.append(folerInfo);
			appendSeps(sb, lengthPerSep + 1);
			int verticalLineOffset = offset + folerInfo.length() + lengthPerSep;

			List<Integer> newVerticalLines = copyOfList0(verticalLines);
			newVerticalLines.add(verticalLineOffset);
			for (int i = 1, len = arr.size(); i < len; i++) {
				JSONObject subObj = arr.optJSONObject(i);
				// switch of 'Obj' or 'Arr'
				if(subObj != null) {
					if (i != 1) {
						sb.append(Tools.CRLF);
						appendVerticalLine(sb, newVerticalLines);
					}
					appendSeps(sb, lengthPerSep);
					sb.append(objInfoExtractor.getTreeObjInfo(subObj) );
					rows ++;
				} else {
					JSONArray subArr = arr.getJSONArray(i);
					sb.append(Tools.CRLF);
					appendVerticalLine(sb, newVerticalLines);
					appendSeps(sb, lengthPerSep);
					appendedRows = tree(subArr, sb, (verticalLineOffset + lengthPerSep), lengthPerSep,
							copyOfList0(newVerticalLines), isAppendCRLFWhileNoFile, objInfoExtractor, arrInfoExtractor);
					// ����˴���ӵ���ӵ�Ԫ�ظ�������1��, ���ӡһ���س� [����鿴]
					rows += appendedRows;
				}
			}
		}

		if (rows == 0) {
			appendSeps(sb, lengthPerSep + 1);
			Tools.append(sb, TREE_NULL);
		}
		if (isAppendCRLFWhileNoFile) {
			if (rows - appendedRows > 0) {
				Tools.append(sb, Tools.CRLF);
				appendVerticalLine(sb, verticalLines);
			}
		}

		return rows;
	}
	
//	// ��ȡ�ļ�, �ļ��е��ַ�����ʾ
//	private static String getFolderString(JSONObject obj) {
//		return obj.getString(TREE_NAME) + "[dir]";
//	}
//	private static String getFileString(JSONObject obj) {
//		return obj.getString(TREE_NAME) + "[" + Tools.getLengthString(obj.getLong(TREE_SIZE), Tools.KB) + "]";
//	}
//	// folder : [{curInfo }, {file}, {file}, .. [folder], [folder], .. ]
//	// ��ȡ��ǰ�ļ�������ĵ�һ��dolder������
//	private static int getFolderStart(JSONArray arr) {
//		for(int i=0, len=arr.size(); i<len; i++) {
//			Object obj = arr.get(i);
//			if(obj instanceof JSONArray) {
//				return i;
//			}
//		}
//		return arr.size();
//	}
	// ���Ƹ���������
	private static List<Integer> copyOfList0(List<Integer> src) {
		  List<Integer> result = new ArrayList<>(src.size() );
		  result.addAll(src);
		  return result;
	}
	// ��ӷָ��
	private static void appendSeps(StringBuilder sb, int lengthPerSep) {
		for(int i=0; i<lengthPerSep; i++) {
			sb.append(TREE_SEPS);
		}
	}
	// ��ӷָ��
	private static void appendVerticalLine(StringBuilder sb, List<Integer> offsets) {
		int last = 0;
		for(Integer off : offsets) {
			appendOffset(sb, off - last);
			sb.append(TREE_VERTICAL_LINE);
			last = off;
		}
	}
	// ��ӷָ��
	private static void appendVerticalLine(StringBuilder sb, int offset) {
		appendOffset(sb, offset);
		sb.append(TREE_VERTICAL_LINE);
	}
	// ���ƫ�ƵĿո�
	private static void appendOffset(StringBuilder sb, int offset) {
		for(int i=0; i<offset; i++) {
			sb.append(" ");
		}
	}
	
}
