/**
 * file name : MsyqlSqlGenerator.java
 * created at : 8:58:47 PM May 23, 2016
 * created by 970655147
 */

package com.hx.log.str;

import com.hx.json.JSONArray;
import com.hx.json.JSONObject;
import com.hx.log.util.Constants;
import com.hx.log.util.Tools;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * mysql相关的sql生成工具
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/5/2017 4:24 PM
 */
public final class MysqlSqlGenerator {

    // disable constructor
    private MysqlSqlGenerator() {
        Tools.assert0("can't instantiate !");
    }

    /**
     * sql的模板
     */
    /**
     * 创建表的sql模板
     */
    public static final String CREATE_TABLE_SQL_TEMPLATE = "CREATE TABLE `%s` (%s) ENGINE=%s AUTO_INCREMENT=%s DEFAULT CHARSET=%s";
    /**
     * 插入的sql模板
     */
    public static final String INSERT_SQL_TEMPLATE = " insert into %s %s values %s ";
    /**
     * 查询的sql模板
     */
    public static final String QUERY_SQL_TEMPLATE = " select %s from %s ";
    /**
     * 更新的sql模板
     */
    public static final String UPDATE_SQL_TEMPLATE = " update %s set %s ";
    /**
     * 删除的sql模板
     */
    public static final String DELETE_SQL_TEMPLATE = " delete from %s ";

    // for decoupe 			--2016.05.25
    /**
     * where 模板
     */
    public static final String WHERE_COND = " where %s ";
    /**
     * limit 模板
     */
    public static final String LIMIT_COND = " limit %s ";
    /**
     * order by 模板
     */
    public static final String SORT_BY_COND = " order by %s ";
    /**
     * group by 模板
     */
    public static final String GROUP_BY_COND = " group by %s ";
    /**
     * 0, 1 模板
     */
    public static String SKIP0_LIMIT1 = "0, 1";
    /**
     * limit 0, 1 模板
     */
    public static String LIMIT1 = String.format(LIMIT_COND, "1");
    //	public static final String SORT_NOTHING = Criteria.sortByNothing().toMysql();
    /**
     * "" 模板
     */
    public static final String SORT_NOTHING = Constants.EMPTY_STR;

    /**
     * kv之间的分隔符
     */
    public static final String SEP = ", ";
    /**
     * 引号
     */
    public static final String QUOTE = "'";
    /**
     * 反引号
     */
    public static final String BACK_QUOTE = "`";

    // 各种类型 [目前支持, String, Int, Double, Boolean]
    /**
     * Boolean 类型
     */
    public static final String TYPE_BOOLEAN = "boolean";
    /**
     * Int 类型
     */
    public static final String TYPE_INT = "int";
    /**
     * Long 类型
     */
    public static final String TYPE_LONG = "long";
    /**
     * Double 类型
     */
    public static final String TYPE_DOUBLE = "double";
    /**
     * String 类型
     */
    public static final String TYPE_STRING = "string";

    // 创建表的各个默认值
    /**
     * 默认的db引擎
     */
    public static String CRT_TABLE_ENGINE = "InnoDB";
    /**
     * 默认的增量
     */
    public static String CRT_TABLE_INCREMENT_START = "1";
    /**
     * 默认的字符集
     */
    public static String CRT_TABLE_CHARSET = "utf8";

    /**
     * 默认的类型 -> 类型声明
     */
    public static Map<String, String> TYPE_2_DECLARE = Tools.asMap(
            new String[]{TYPE_STRING, TYPE_INT, TYPE_LONG, TYPE_DOUBLE, TYPE_BOOLEAN},
            "varchar(60)", "int(11)", "int(20)", "double", "tinyint(1)");

    /**
     * 生成创建表的语句
     *
     * @param table   给定的表
     * @param beanObj 需要创建的表的一行数据
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:31 PM
     * @since 1.0
     */
    public static String generateCreateTableSql(String table, JSONObject beanObj) {
        Tools.assert0(table != null, "table can't be null !");
        Tools.assert0(beanObj != null, "beanObj can't be null !");

        StringBuilder sb = new StringBuilder();
        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        for (Object _key : beanObj.names()) {
            String key = (String) _key;
            String type = defineType(beanObj.getString(key));
            Tools.appendCRLF(sb, "	`" + key + "` " + TYPE_2_DECLARE.get(type) + SEP);
        }
        sb.append("	PRIMARY KEY (`id`) ");
        Tools.appendCRLF(sb, Tools.EMPTY_STR);

        return String.format(CREATE_TABLE_SQL_TEMPLATE, table, sb.toString(), CRT_TABLE_ENGINE, CRT_TABLE_INCREMENT_START, CRT_TABLE_CHARSET);
    }

    /**
     * 根据给定的"值", 尝试判断类型
     *
     * @param value 给定的值
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:31 PM
     * @since 1.0
     */
    public static String defineType(String value) {
        Tools.assert0(value != null, "value can't be null !");

        String lowerValue = value.toLowerCase();
        if (Constants.TRUE.equals(lowerValue) || Constants.FALSE.equals(lowerValue)) {
            return TYPE_BOOLEAN;
        }
        try {
            Integer.parseInt(value);
            return TYPE_INT;
        } catch (Exception eI) {
            try {
                Long.parseLong(value);
                return TYPE_LONG;
            } catch (Exception eL) {
                try {
                    Double.parseDouble(value);
                    return TYPE_DOUBLE;
                } catch (Exception eD) {
                    // ignore
                }
            }
        }

        return TYPE_STRING;
    }

    /**
     * 生成插入数据的sql
     *
     * @param table    给定的表名
     * @param beanObjs 需要插入的bean列表
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:32 PM
     * @since 1.0
     */
    public static String generateInsertSql(String table, List<JSONObject> beanObjs) {
        Tools.assert0(table != null, "table can't be null !");
        Tools.assert0(beanObjs != null, "beanObjs can't be null !");

        StringBuilder colNames = new StringBuilder();
        // (name, pwd, email)
        JSONArray names = beanObjs.get(0).names();
        colNames.append("(");
        for (Object nameObj : names) {
            colNames.append(BACK_QUOTE);
            colNames.append(transferInput((String) nameObj));
            colNames.append(BACK_QUOTE);
            colNames.append(SEP);
        }
        Tools.removeLastSep(colNames, SEP);
        colNames.append(")");

        // ('hx', 'hxPwd', 'email')
        // 全部以字符串的形式插入
        // 这样就不能使用insert into (col01, col02) cols values (2, col01 * 3)了,,
        StringBuilder values = new StringBuilder();
        for (JSONObject beanObj : beanObjs) {
            values.append("(");
            for (Object nameObj : names) {
                values.append(QUOTE);
                values.append(transferInput(beanObj.getString((String) nameObj)));
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

    public static String generateInsertSql(String table, JSONObject beanObj) {
        return generateInsertSql(table, Arrays.asList(beanObj));
    }

    /**
     * 生成查询数据的sql
     *
     * @param table      需要查询的表
     * @param projection 需要查询的字段
     * @param cond       查询的条件
     * @param limit      limit
     * @param sort       sort
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:32 PM
     * @since 1.0
     */
    public static String generateQuerySql(String table, String projection, String cond, String limit, String sort) {
        Tools.assert0(table != null, "table can't be null !");
        Tools.assert0(projection != null, "projection can't be null !");

        StringBuilder querySqlTemplate = new StringBuilder(QUERY_SQL_TEMPLATE);
        List<String> args = Tools.asList(projection, table);
        if (!Tools.isEmpty(cond)) {
            querySqlTemplate.append(WHERE_COND);
            args.add(cond);
        }
        if (!Tools.isEmpty(sort)) {
            querySqlTemplate.append(SORT_BY_COND);
            args.add(sort);
        }
        if (!Tools.isEmpty(limit)) {
            querySqlTemplate.append(LIMIT_COND);
            args.add(limit);
        }

        return String.format(querySqlTemplate.toString(), args.toArray());
    }

    /**
     * 生成group 的语句
     * reduce					  key		 cond
     * select * from person group by name where age > 20;
     *
     * @param table      表名
     * @param projection 需要查询的字段
     * @param cond       查询的条件
     * @param groupBy    group by 的字段列表
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:33 PM
     * @since 1.0
     */
    public static String generateGroupBySql(String table, String projection, String cond, String groupBy) {
        Tools.assert0(table != null, "table can't be null !");
        Tools.assert0(projection != null, "projection can't be null !");

        StringBuilder querySqlTeplate = new StringBuilder(QUERY_SQL_TEMPLATE);
        List<String> args = Tools.asList(projection, table);
        if (!Tools.isEmpty(groupBy)) {
            querySqlTeplate.append(GROUP_BY_COND);
            args.add(groupBy);
        }
        if (!Tools.isEmpty(cond)) {
            querySqlTeplate.append(WHERE_COND);
            args.add(cond);
        }

        return String.format(querySqlTeplate.toString(), args.toArray());
    }

    /**
     * 查询满足给定的条件的一条记录
     *
     * @param table      给定的表
     * @param projection 需要查询的字段
     * @param cond       查询的条件
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:34 PM
     * @since 1.0
     */
    public static String generateQueryOneSql(String table, String projection, String cond) {
        return generateQuerySql(table, projection, cond, SKIP0_LIMIT1, SORT_NOTHING);
    }

    /**
     * 生成更新数据的sql
     *
     * @param table  给定的表名
     * @param update 需要更新的语句
     * @param cond   更新的条件
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:35 PM
     * @since 1.0
     */
    public static String generateUpdateSql(String table, String update, String cond) {
        Tools.assert0(table != null, "table can't be null !");
        Tools.assert0(update != null, "update can't be null !");

        if (Tools.isEmpty(cond)) {
            return String.format(UPDATE_SQL_TEMPLATE, table, update);
        } else {
            return String.format(UPDATE_SQL_TEMPLATE + WHERE_COND, table, update, cond);
        }
    }

    /**
     * 生成更新符合条件的第一条数据的sql
     *
     * @param table  给定的表名
     * @param update 需要更新的语句
     * @param cond   更新的条件
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:35 PM
     * @since 1.0
     */
    public static String generateUpdateOneSql(String table, String update, String cond) {
        Tools.assert0(table != null, "table can't be null !");
        Tools.assert0(update != null, "update can't be null !");

        if (Tools.isEmpty(cond)) {
            return String.format(UPDATE_SQL_TEMPLATE + LIMIT1, table, update);
        } else {
            return String.format(UPDATE_SQL_TEMPLATE + WHERE_COND + LIMIT1, table, update, cond);
        }
    }

    /**
     * 生成删除数据的sql
     *
     * @param table 给定的表名
     * @param cond  删除数据的条件
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:35 PM
     * @since 1.0
     */
    public static String generateDeleteSql(String table, String cond) {
        Tools.assert0(table != null, "table can't be null !");

        if (Tools.isEmpty(cond)) {
            return String.format(DELETE_SQL_TEMPLATE, table);
        } else {
            return String.format(DELETE_SQL_TEMPLATE + WHERE_COND, table, cond);
        }
    }

    /**
     * 生成删除第一条数据的sql
     *
     * @param table 给定的表名
     * @param cond  删除数据的条件
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:35 PM
     * @since 1.0
     */
    public static String generateDeleteOneSql(String table, String cond) {
        Tools.assert0(table != null, "table can't be null !");

        if (Tools.isEmpty(cond)) {
            return String.format(DELETE_SQL_TEMPLATE + LIMIT1, table);
        } else {
            return String.format(DELETE_SQL_TEMPLATE + WHERE_COND + LIMIT1, table, cond);
        }
    }

    /**
     * 转义给定的输入, 目前 需要转义 '
     * to be continued !
     *
     * @param content content
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 6/25/2017 10:42 AM
     * @since 1.0
     */
    private static String transferInput(String content) {
        return StringUtils.transfer(content, Tools.asSet('\'', '"'), '\\');
    }


}
