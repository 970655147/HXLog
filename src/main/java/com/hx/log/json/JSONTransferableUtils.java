/**
 * file name : JSONTransferableUtils.java
 * created at : 4:54:43 PM May 22, 2016
 * created by 970655147
 */

package com.hx.log.json;

import com.hx.common.util.ReflectUtils;
import com.hx.json.JSONArray;
import com.hx.json.JSONObject;
import com.hx.log.util.Constants;
import com.hx.log.util.Log;
import com.hx.log.util.Tools;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.*;
import java.util.Map.Entry;

/**
 * 生成给定的Class的JSONTransferable的索引
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/5/2017 4:17 PM
 */
public final class JSONTransferableUtils {

    // disable constructor
    private JSONTransferableUtils() {
        Tools.assert0("can't instantiate !");
    }

    // 根据给定的Class写出如下模板
//	// for debug
//	public String toString() {
//		return encapJSON(Constants.doLoadNormalNothingIdxMap, Constants.doFilterNothingFilterMap ).toString();
//	}
//
//	// loadFromObject相关索引
//	public static final int NORMAL = 0;
//	public static final String[] nameIdxes = {"name" }; 
//	public static final String[] pwdIdxes = {"pwd" }; 
//	public static final String[] friendsIdxes = {"friends" }; 
//	public static final String[] niceIdxes = {"nice" }; 
//
//	// encapJSON相关filter
//	public static final int ALL = 0;
//	public static final List<Set<String>> filters = Tools.asList(Tools.asSet("") );
//
//	public static final String BEAN_KEY = "user_key";
//	public static final User PROTO_BEAN = new User();
//
//	@Override
//	public User loadFromJSON(Map<String, Object> obj, Map<String, Integer> idxMap) {
//		if(Tools.isEmpty(idxMap) || (idxMap.get(BEAN_KEY) == null) ) {
//			return this;
//		}
//		int idx = idxMap.get(BEAN_KEY).intValue();
//
//		this.name = Tools.getString(obj, idx, nameIdxes);
//		this.pwd = Tools.getString(obj, idx, pwdIdxes);
//
//		JSONArray friendsArr = Tools.optJSONArray(obj, idx, friendsIdxes);
//		this.friends = new ArrayList(0);
//		if(! Tools.isEmpty(friendsArr) ) {
//			this.friends = new ArrayList(friendsArr.size() );
//			for(int i=0; i<friendsArr.size(); i++) {
//				this.friends.add(new User().loadFromJSON(Tools.getJSONObject(friendsArr, i), idxMap) );
//			}
//		}
//
//		JSONArray niceArr = Tools.optJSONArray(obj, idx, niceIdxes);
//		this.nice = new HashSet();
//		if(! Tools.isEmpty(niceArr) ) {
//			for(int i=0; i<niceArr.size(); i++) {
//				this.nice.add(new User().loadFromJSON(Tools.getJSONObject(niceArr, i), idxMap) );
//			}
//		}
//
//		return this;
//	}
//
//	@Override
//	public JSONObject encapJSON(Map<String, Integer> idxMap, Map<String, Integer> filterIdxMap) {
//		if(Tools.isEmpty(idxMap) || (idxMap.get(BEAN_KEY) == null) ) {
//			return null;
//		}
//		int idx = idxMap.get(BEAN_KEY).intValue();
//
//		JSONArray friendsArr = new JSONArray();
//		if(! Tools.isEmpty(this.friends) ) {
//			for(User ele : this.friends) {
//				friendsArr.add(ele.encapJSON(idxMap, filterIdxMap) );
//			}
//		}
//
//		JSONArray niceArr = new JSONArray();
//		if(! Tools.isEmpty(this.nice) ) {
//			for(User ele : this.nice) {
//				niceArr.add(ele.encapJSON(idxMap, filterIdxMap) );
//			}
//		}
//
//		JSONObject res = new JSONObject()
//			.element(nameIdxes[Tools.getIdx(idx, nameIdxes)], name).element(pwdIdxes[Tools.getIdx(idx, pwdIdxes)], pwd).element(friendsIdxes[Tools.getIdx(idx, friendsIdxes)], friendsArr)
//			.element(niceIdxes[Tools.getIdx(idx, niceIdxes)], niceArr);
//
//		if(Tools.isEmpty(filterIdxMap) || (filterIdxMap.get(BEAN_KEY) == null) ) {
//			return res;
//		}
//		int filterIdx = filterIdxMap.get(BEAN_KEY).intValue();
//		return Tools.filter(res, filters.get(Tools.getIdx(filterIdx, filters.size())) );
//	}
//
//	@Override
//	public User newInstance(Object... args) {
//		return new User();
//	}

    // 使用的工具类
    /**
     * 工具类的类名
     */
    public static String UTILS = Constants.optString(JSONTransferableUtilsConstants._JSON_TUTILS);
    /**
     * id的名称
     */
    public static String ID = Constants.optString(JSONTransferableUtilsConstants._JSON_TID);
    /**
     * PROTO_BEAN的名称
     */
    public static String PROTO_BEAN_KEY = Constants.optString(JSONTransferableUtilsConstants._JSON_TPROTO_BEAN_KEY);

    /**
     * 生成JSONTransferable的相关方法
     *
     * @param utils       工具类名称
     * @param clazz       给定的需要生成的类的class
     * @param elemPerLine loadFromJSON, encapJSON 每一行多少个element
     * @param prefix      前缀, 如果有的话, 生成另外两套带前缀的idx
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:04 PM
     * @since 1.0
     */
    public static String generateIdxes(String utils, Class clazz, int elemPerLine, String prefix) throws Exception {
        StringBuilder sb = new StringBuilder();
        Field[] fields = clazz.getDeclaredFields();
        fields = collectMemberFields(fields);

        fieldsDeclare(sb, fields);

        // @Override toString()
        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "// for debug");
        Tools.appendCRLF(sb, "@Override");
        Tools.appendCRLF(sb, "public String toString() {");
        toString(sb);
        Tools.appendCRLF(sb, "}");

        // BEAN_KEY, protoBean
        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "public static final " + clazz.getSimpleName() + " " + getProtoBeanKey(clazz) + " = new " + clazz.getSimpleName() + "();");

        // @Override public BeanType loadFromJSON(JSONObject obj, Map<String, Integer> idxMap)
        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "@Override");
        Tools.appendCRLF(sb, "public " + clazz.getSimpleName() + " loadFromJSON(Map<String, Object> obj, JSONConfig config) {");
        loadFromJson(sb, utils, clazz, fields);
        Tools.appendCRLF(sb, "}");

        // @Override public JSONObject encapJSON(Map<String, Integer> idxMap, Map<String, Integer> filterIdxMap)
        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "@Override");
        Tools.appendCRLF(sb, "public JSONObject encapJSON(JSONConfig config) {");
        encapJson(sb, utils, elemPerLine, clazz, fields);
        Tools.appendCRLF(sb, "}");

        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "@Override");
        Tools.appendCRLF(sb, "public JSONObject encapJSON(JSONConfig config, Deque<Object> cycleDectector) {");
        encapJsonWithDectector(sb, utils, elemPerLine, clazz, fields);
        Tools.appendCRLF(sb, "}");

        // @Override public BeanType newInstance()
        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "@Override");
        Tools.appendCRLF(sb, "public " + clazz.getSimpleName() + " newInstance(Object... args) {");
        newInstance(sb, clazz);
        Tools.appendCRLF(sb, "}");

        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "@Override");
        Tools.appendCRLF(sb, "public String id() {");
        id(sb, clazz);
        Tools.appendCRLF(sb, "}");

        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "@Override");
        Tools.appendCRLF(sb, "public void id(String id) {");
        idSetter(sb, clazz);
        Tools.appendCRLF(sb, "}");

        return sb.toString();
    }

    public static String generateIdxes(Class clazz, int elemPerLine, String prefix) throws Exception {
        return generateIdxes(UTILS, clazz, elemPerLine, prefix);
    }

    public static String generateIdxes(Class clazz, int elemPerLine) throws Exception {
        return generateIdxes(UTILS, clazz, elemPerLine);
    }

    public static String generateIdxes(String utils, Class clazz, int elemPerLine) throws Exception {
        return generateIdxes(utils, clazz, elemPerLine, Tools.EMPTY_STR);
    }

    public static String generateFieldsDecoare(String utils, Class clazz, String prefix, String suffix) throws Exception {
        StringBuilder sb = new StringBuilder();
        Field[] fields = clazz.getDeclaredFields();
        fields = collectMemberFields(fields);

        fieldsDeclare(sb, fields, prefix, suffix);
        return sb.toString();
    }
    public static String generateFieldsDecoare(String utils, Class clazz) throws Exception {
        return generateFieldsDecoare(utils, clazz, null, null);
    }

//	// AdvInfoDao
//	static interface MysqlAdvInfoDao extends MysqlIBaseDao<AdvInfo> {
//		
//	}
//	// MysqlAdvInfoDaoImpl
//	static class MysqlAdvInfoDaoImpl extends MysqlBaseDaoImpl<AdvInfo> implements MysqlAdvInfoDao {
//
//		public MysqlAdvInfoDaoImpl(AdvInfo bean) {
//			super(bean, (MysqlDbConfig) new MysqlDbConfig().ID("_id") );
//		}
//		public MysqlAdvInfoDaoImpl(AdvInfo bean, MysqlDbConfig config) {
//			super(bean, config);
//		}
//		
//	}


    /**
     * 生成dao的类型 - mysql
     */
    public static final String TYPE_MYSQL = "Mysql";
    /**
     * 生成dao的类型 - mongo
     */
    public static final String TYPE_MONGO = "Mongo";
    /**
     * 生成dao, daoImpl的注释声明
     */
    public static String preDaoDaoImpl = "// ----------------------- dao & daoImpl----------------------------";
    /**
     * type -> ConnectionType
     */
    public static final Map<String, Object> type2ConnectionType = new JSONObject().element(TYPE_MYSQL, "Connection").element(TYPE_MONGO, "MongoClient");


    /**
     * 获取最简单的XXXDao, XXXDaoImpl
     *
     * @param clazz 给定的class
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:07 PM
     * @since 1.0
     */
    public static String generateAllDaoDaoImpl(Class clazz) {
        StringBuilder sb = new StringBuilder();
        Tools.appendCRLF(sb, preDaoDaoImpl);
        for (Entry<String, Object> entry : type2ConnectionType.entrySet()) {
            Tools.appendCRLF(sb, generateDaoDaoImpl(clazz, entry.getKey()));
        }

        return sb.toString();
    }

    /**
     * 根据给定的class, 生成给定的类型的dao实现
     *
     * @param clazz 给定的class
     * @param type  需要生成的dao的类型
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:08 PM
     * @since 1.0
     */
    public static String generateDaoDaoImpl(Class clazz, String type) {
        if (!type2ConnectionType.containsKey(type)) {
            String debugInfo = "unKnown type : '" + type + "'";
            Log.err(debugInfo);
            return debugInfo;
        }

        StringBuilder sb = new StringBuilder();

        String clazzName = clazz.getSimpleName();
        String daoName = (type + clazzName) + "Dao";
        String daoImplName = (type + clazzName) + "DaoImpl";
        Tools.appendCRLF(sb, "// " + daoName);
        Tools.appendCRLF(sb, "public static interface " + daoName + " extends " + type + "IBaseDao<" + clazzName + "> {");
        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "}");
        Tools.appendCRLF(sb, "// " + daoImplName);
        Tools.appendCRLF(sb, "public static class " + daoImplName + " extends " + type + "BaseDaoImpl<" + clazzName + "> implements " + daoName + " {");
        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "	public " + daoImplName + "(" + clazzName + " bean) {");
        Tools.appendCRLF(sb, "		super(bean);");
        Tools.appendCRLF(sb, "	}");
        Tools.appendCRLF(sb, "	public " + daoImplName + "(" + clazzName + " bean, DbConfig config) {");
        Tools.appendCRLF(sb, "		super(bean, config);");
        Tools.appendCRLF(sb, "	}");
        Tools.appendCRLF(sb, "	public " + daoImplName + "(" + clazzName + " bean, ConnectionProvider<" + type2ConnectionType.get(type) + "> connectionProvider) {");
        Tools.appendCRLF(sb, "		super(bean, connectionProvider);");
        Tools.appendCRLF(sb, "	}");
        Tools.appendCRLF(sb, "	public " + daoImplName + "(" + clazzName + " bean, DbConfig config, ConnectionProvider<" + type2ConnectionType.get(type) + "> connectionProvider) {");
        Tools.appendCRLF(sb, "		super(bean, config, connectionProvider);");
        Tools.appendCRLF(sb, "	}");
        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "}");

        return sb.toString();
    }

    // --------------- 辅助方法 --------------------

    /**
     * toString 方法的输出
     *
     * @param sb 给定的需要输出的sb
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static void toString(StringBuilder sb) {
//        Tools.appendCRLF(sb, "	return String.valueOf(encapJSON(" + IDX_MAP_MANAGER + ".doLoadNormalNothingIdxMap, " + IDX_MAP_MANAGER + ".doFilterNothingFilterMap) );");
//		Tools.appendCRLF(sb, "	return encapJSON(new JSONObject().element(BEAN_KEY(), defaultLoadIdx() ), new JSONObject().element(BEAN_KEY(), DEFAULT_FILTER_IDX()) ).toString();" );
//		Tools.appendCRLF(sb, toStringDeclare);
        Tools.appendCRLF(sb, "	return JSONObject.fromObject(this).toString();");
    }

    /**
     * loadFromJson
     *
     * @param sb 给定的需要输出的sb
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static void loadFromJson(StringBuilder sb, String utils, Class clazz, Field[] fields) throws Exception {
        Tools.appendCRLF(sb, "	if (Tools.isEmpty(obj)) {");
        Tools.appendCRLF(sb, "	    return this;");
        Tools.appendCRLF(sb, "	}");
        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "	JSONObject.fromObject(obj).toBean(" + clazz.getSimpleName() + ".class, this, config);");
        Tools.appendCRLF(sb, "	return this;");
    }

    /**
     * encapJson
     *
     * @param sb 给定的需要输出的sb
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static void encapJson(StringBuilder sb, String utils, int elemPerLine, Class clazz, Field[] fields) throws Exception {
        Tools.appendCRLF(sb, "	return encapJSON(config, new LinkedList<Object>() );");
    }

    /**
     * encapJson
     *
     * @param sb 给定的需要输出的sb
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static void encapJsonWithDectector(StringBuilder sb, String utils, int elemPerLine, Class clazz, Field[] fields) throws Exception {
        Tools.appendCRLF(sb, "	if(cycleDectector.contains(this) ) {");
        Tools.appendCRLF(sb, "		return JSONObject.fromObject(Constants.OBJECT_ALREADY_EXISTS).element(\"id\", String.valueOf(id()) );");
        Tools.appendCRLF(sb, "	}");
        Tools.appendCRLF(sb, "	cycleDectector.push(this);");

        Tools.appendCRLF(sb, Tools.EMPTY_STR);
        Tools.appendCRLF(sb, "	return JSONObject.fromObject(this, config);");
    }

    /**
     * newInstance
     *
     * @param sb 给定的需要输出的sb
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static void newInstance(StringBuilder sb, Class clazz) {
        Tools.appendCRLF(sb, "	return new " + clazz.getSimpleName() + "();");
    }

    /**
     * id
     *
     * @param sb 给定的需要输出的sb
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static void id(StringBuilder sb, Class clazz) {
        Tools.appendCRLF(sb, "	return " + ID + ";");
    }

    /**
     * id
     *
     * @param sb 给定的需要输出的sb
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:09 PM
     * @since 1.0
     */
    private static void idSetter(StringBuilder sb, Class clazz) {
        Tools.appendCRLF(sb, "	this." + ID + " = id;");
    }

    /**
     * 获取给定的所有的字段的 声明[包含注解]
     *
     * @param sb sb
     * @param fields fields
     * @return void
     * @author Jerry.X.He
     * @date 5/29/2017 1:11 PM
     * @since 1.0
     */
    private static void fieldsDeclare(StringBuilder sb, Field[] fields, String prefix, String suffix) {
        for(Field field : fields) {
            JSONArray arr = new JSONArray();
            String camelDeclare = field.getName();
            String underLineDeclare = Tools.camel2UnderLine(field.getName());

            arr.add(camelDeclare);
            arr.add(underLineDeclare);
            if(! Tools.isEmpty(prefix)) {
                arr.add(prefix + camelDeclare);
                arr.add(prefix + underLineDeclare);
            }
            if(! Tools.isEmpty(suffix)) {
                arr.add(camelDeclare + suffix);
                arr.add(underLineDeclare + suffix);
            }

            String jsonFields = arr.toString();
            jsonFields = jsonFields.substring(1, jsonFields.length()-1);
            Tools.appendCRLF(sb, "@JSONField({" + jsonFields + "})");
            Tools.appendCRLF(sb, "private " + field.getType().getSimpleName() + " " + field.getName() + ";");
        }
    }

    private static void fieldsDeclare(StringBuilder sb, Field[] fields) {
        fieldsDeclare(sb, fields, null, null);
    }

    /**
     * 获取给定的class的PROTO_BEAN_KEY
     *
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:12 PM
     * @since 1.0
     */
    private static String getProtoBeanKey(Class clazz) {
        return PROTO_BEAN_KEY;
    }

    /**
     * 去掉给定的列表中的不属于成员字段的成员
     *
     * @param fields fields
     * @return java.lang.reflect.Field[]
     * @author Jerry.X.He
     * @date 5/20/2017 11:18 AM
     * @since 1.0
     */
    private static Field[] collectMemberFields(Field[] fields) {
        List<Field> memeberFields = new ArrayList<>(fields.length);
        for (Field field : fields) {
            int mod = field.getModifiers();
            if (!Modifier.isStatic(mod)) {
                memeberFields.add(field);
            }
        }

        Field[] result = new Field[memeberFields.size()];
        memeberFields.toArray(result);
        return result;
    }

}
