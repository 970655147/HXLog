/**
 * file name : MsyqlSqlGenerator.java
 * created at : 8:58:47 PM May 23, 2016
 * created by 970655147
 */

package com.hx.log.str;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import com.hx.log.util.Constants;
import com.hx.log.util.Tools;
import com.hx.json.JSONArray;
import com.hx.json.JSONObject;

public final class MysqlSqlGenerator {

	// disable constructor
	private MysqlSqlGenerator() {
		Tools.assert0("can't instantiate !");
	}
	
	// sql��ģ��
	public static final String CREATE_TABLE_SQL_TEMPLATE = "CREATE TABLE `%s` (%s) ENGINE=%s AUTO_INCREMENT=%s DEFAULT CHARSET=%s";
	public static final String INSERT_SQL_TEMPLATE = " insert into %s %s values %s ";
	public static final String QUERY_SQL_TEMPLATE = " select %s from %s ";
	public static final String UPDATE_SQL_TEMPLATE = " update %s set %s ";
	public static final String DELETE_SQL_TEMPLATE = " delete from %s ";
	
	// for decoupe 			--2016.05.25
	public static final String WHERE_COND = " where %s ";
	public static final String LIMIT_COND = " limit %s ";
	public static final String SORT_BY_COND = " order by %s ";
	public static final String GROUP_BY_COND = " group by %s ";
	public static final String SKIP0_LIMIT1 = "0, 1";
	public static final String LIMIT1 = String.format(LIMIT_COND, SKIP0_LIMIT1);
//	public static final String SORT_NOTHING = Criteria.sortByNothing().toMysql();
	public static final String SORT_NOTHING = Constants.EMPTY_STR;

	public static final String SEP = ", ";
	public static final String QUOTE = "'";
	public static final String BACK_QUOTE = "`";

	// �������� [Ŀǰ֧��, String, Int, Double, Boolean]
	public static final String TYPE_STRING = "string";
	public static final String TYPE_INT = "int";
	public static final String TYPE_LONG = "long";
	public static final String TYPE_DOUBLE = "double";
	public static final String TYPE_BOOLEAN = "boolean";
	
	// ������ĸ���Ĭ��ֵ
	public static String CRT_TABLE_ENGINE = "InnoDB";
	public static String CRT_TABLE_INCREMENT_START = "1";
	public static String CRT_TABLE_CHARSET = "utf8";
	
	// Ĭ�ϵ����� -> ��������
	public static Map<String, String> TYPE_2_DECLARE = Tools.asMap(
			new String[]{TYPE_STRING, TYPE_INT, TYPE_LONG, TYPE_DOUBLE, TYPE_BOOLEAN },
			new String[]{"varchar(60)", "int(11)", "int(20)", "double", "tinyint(1)" } );
	
	// ���ɴ���������
	public static String generateCreateTableSql(String table, JSONObject beanObj) {
		Tools.assert0(table != null, "table can't be null !");
		Tools.assert0(beanObj != null, "beanObj can't be null !");
		
		StringBuilder sb = new StringBuilder();
		Tools.appendCRLF(sb, Tools.EMPTY_STR);
		for(Object _key : beanObj.names() ) {
			String key = (String) _key;
			String type = defineType(beanObj.getString(key) );
			Tools.appendCRLF(sb, "	`" + key + "` " + TYPE_2_DECLARE.get(type) + SEP);
		}
		sb.append("	PRIMARY KEY (`id`)");
		Tools.appendCRLF(sb, Tools.EMPTY_STR);
		
		return String.format(CREATE_TABLE_SQL_TEMPLATE, table, sb.toString(), CRT_TABLE_ENGINE, CRT_TABLE_INCREMENT_START, CRT_TABLE_CHARSET);
	}
	// ���ݸ�����"ֵ", �����ж�����
	public static String defineType(String value) {
		Tools.assert0(value != null, "value can't be null !");
		
		String lowerValue = value.toLowerCase();
		if(Constants.TRUE.equals(lowerValue) || Constants.FALSE.equals(lowerValue) ) {
			return TYPE_BOOLEAN;
		}
		try {
			Integer.parseInt(value);
			return TYPE_INT;
		} catch (Exception eI) {
			try {
				Long.parseLong(value);
				return TYPE_LONG;
			} catch(Exception eL) {
				try {
					Double.parseDouble(value);
					return TYPE_DOUBLE;
				} catch(Exception eD) {
					// ignore
				}
			}
		}
		
		return TYPE_STRING;
	}
	
	// ���ɲ������ݵ�sql
	public static String generateInsertSql(String table, JSONObject beanObj) {
		return generateInsertSql(table, Arrays.asList(beanObj) );
	}
	public static String generateInsertSql(String table, List<JSONObject> beanObjs) {
		Tools.assert0(table != null, "table can't be null !");
		Tools.assert0(beanObjs != null, "beanObjs can't be null !");
		
		StringBuilder colNames = new StringBuilder();
		// (name, pwd, email)
		JSONArray names = beanObjs.get(0).names();
		colNames.append("(");
		for(Object nameObj : names) {
			colNames.append(BACK_QUOTE);
			colNames.append((String) nameObj );
			colNames.append(BACK_QUOTE);
			colNames.append(SEP);
		}
		Tools.removeLastSep(colNames, SEP);
		colNames.append(")");
		
		// ('hx', 'hxPwd', 'email')
		// ȫ�����ַ�������ʽ����
			// �����Ͳ���ʹ��insert into (col01, col02) cols values (2, col01 * 3)��,,
		StringBuilder values = new StringBuilder();
		for(JSONObject beanObj : beanObjs) {
			values.append("(");
			for(Object nameObj : names) {
				values.append(QUOTE );
				values.append(beanObj.getString((String) nameObj) );
				values.append(QUOTE);
				values.append(SEP);
			}
			Tools.removeLastSep(values, SEP);
			values.append(")");
			values.append(SEP);
		}
		Tools.removeLastSep(values, SEP);
		
		return String.format(INSERT_SQL_TEMPLATE, table, colNames.toString(), values);
	}
	
	// ���ɲ�ѯ���ݵ�sql
	public static String generateQuerySql(String table, String projection, String cond, String limit, String sort) {
		Tools.assert0(table != null, "table can't be null !");
		Tools.assert0(projection != null, "projection can't be null !");
		
		StringBuilder querySqlTemplate = new StringBuilder(QUERY_SQL_TEMPLATE);
		List<String> args = Tools.asList(projection, table);
		if(! Tools.isEmpty(cond) ) {
			querySqlTemplate.append(WHERE_COND);
			args.add(cond);
		}
		if(! Tools.isEmpty(sort) ) {
			querySqlTemplate.append(SORT_BY_COND);
			args.add(sort);
		}
		if(! Tools.isEmpty(limit) ) {
			querySqlTemplate.append(LIMIT_COND);
			args.add(limit);
		}
		
		return String.format(querySqlTemplate.toString(), args.toArray() );
	}
	// 		reduce					  key		 cond
	// select * from person group by name where age > 20;
	public static String generateGroupBySql(String table, String projection, String cond, String groupBy) {
		Tools.assert0(table != null, "table can't be null !");
		Tools.assert0(projection != null, "projection can't be null !");
		
		StringBuilder querySqlTeplate = new StringBuilder(QUERY_SQL_TEMPLATE);
		List<String> args = Tools.asList(projection, table);
		if(! Tools.isEmpty(groupBy) ) {
			querySqlTeplate.append(GROUP_BY_COND);
			args.add(groupBy);
		}
		if(! Tools.isEmpty(cond) ) {
			querySqlTeplate.append(WHERE_COND);
			args.add(cond);
		}
		
		return String.format(querySqlTeplate.toString(), args.toArray() );
	}
	public static String generateQueryOneSql(String table, String projection, String cond) {
		return generateQuerySql(table, projection, cond, SKIP0_LIMIT1, SORT_NOTHING);
	}
	
	// ���ɸ������ݵ�sql
	public static String generateUpdateSql(String table, String update, String cond) {
		Tools.assert0(table != null, "table can't be null !");
		Tools.assert0(update != null, "update can't be null !");
		
		if(Tools.isEmpty(cond) ) {
			return String.format(UPDATE_SQL_TEMPLATE, table, update);
		} else {
			return String.format(UPDATE_SQL_TEMPLATE + WHERE_COND, table, update, cond);
		}
	}
	public static String generateUpdateOneSql(String table, String update, String cond) {
		Tools.assert0(table != null, "table can't be null !");
		Tools.assert0(update != null, "update can't be null !");
		
		if(Tools.isEmpty(cond) ) {
			return String.format(UPDATE_SQL_TEMPLATE + LIMIT1, table, update);
		} else {
			return String.format(UPDATE_SQL_TEMPLATE + WHERE_COND + LIMIT1, table, update, cond);
		}
	}
	
	// ����ɾ�����ݵ�sql
	public static String generateDeleteSql(String table, String cond) {
		Tools.assert0(table != null, "table can't be null !");
		
		if(Tools.isEmpty(cond) ) {
			return String.format(DELETE_SQL_TEMPLATE, table);
		} else {
			return String.format(DELETE_SQL_TEMPLATE + WHERE_COND, table, cond);
		}
	}
	public static String generateDeleteOneSql(String table, String cond) {
		Tools.assert0(table != null, "table can't be null !");
		
		if(Tools.isEmpty(cond) ) {
			return String.format(DELETE_SQL_TEMPLATE + LIMIT1, table);
		} else {
			return String.format(DELETE_SQL_TEMPLATE + WHERE_COND + LIMIT1, table, cond);
		}
	}
	

}
