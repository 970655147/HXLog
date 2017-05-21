package com.hx.log.util;

import com.hx.json.JSONParseUtils;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

/**
 * BeanTransferUtils
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/21/2017 4:27 PM
 */
public final class BeanTransferUtils {

    // disable constructor
    private BeanTransferUtils() {
        Tools.assert0("can't instantiate !");
    }

    /**
     * 一个src的实例, 转换到dst实例需要做的事情
     *
     * @param src src
     * @param dst dst
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/21/2017 4:28 PM
     * @since 1.0
     */
    public static String transferTo(Class src, Class dst) {
        Tools.assert0(src != null, "'src' can't be null !");
        Tools.assert0(dst != null, "'dst' can't be null !");

        String srcClazzName = src.getSimpleName();
        String dstClazzName = dst.getSimpleName();
        StringBuilder sb = new StringBuilder();

        Tools.appendCRLF(sb, "public static " + dstClazzName + " " + getTransferMethodName(srcClazzName, dstClazzName) + "(" + srcClazzName + " src) { ");
        Tools.appendCRLF(sb, "    " + dstClazzName + " result = new " + dstClazzName + "();");
        Method[] srcMethods = src.getDeclaredMethods();
        for (Method method : srcMethods) {
            int mod = method.getModifiers();
            if ((!Modifier.isStatic(mod))
                    && JSONParseUtils.startsWith(method.getName(), Constants.BEAN_GETTER_PREFIXES)) {
                String attr = JSONParseUtils.trimIfStartsWith(method.getName(), Constants.BEAN_GETTER_PREFIXES);
                Method setter = lookForSetterMethod(dst, attr);
                if(setter != null) {
                    Tools.appendCRLF(sb, "    result." + setter.getName() + "(src." + method.getName() + "());");
                }
            }
        }
        Tools.appendCRLF(sb, "    return result;");
        Tools.appendCRLF(sb, "}");

        return sb.toString();
    }

    public static String transferListTo(Class src, Class dst) {
        Tools.assert0(src != null, "'src' can't be null !");
        Tools.assert0(dst != null, "'dst' can't be null !");

        String srcClazzName = src.getSimpleName();
        String dstClazzName = dst.getSimpleName();
        StringBuilder sb = new StringBuilder();

        Tools.appendCRLF(sb, "public static Collection<" + dstClazzName + "> " + getTransferMethodName(srcClazzName, dstClazzName) + "List(Collection<" + srcClazzName + "> src) { ");
        Tools.appendCRLF(sb, "    List<" + dstClazzName + "> result = new ArrayList<>(src.size());");
        Tools.appendCRLF(sb, "    for(" + srcClazzName + " ele : src) {");
        Tools.appendCRLF(sb, "        result.add(" + getTransferMethodName(srcClazzName, dstClazzName) + "(ele));");
        Tools.appendCRLF(sb, "    }");
        Tools.appendCRLF(sb, "    return result;");
        Tools.appendCRLF(sb, "}");

        return sb.toString();
    }

    /**
     * 获取给定的class的属性的setter
     *
     * @param clazz clazz
     * @param attr  attr
     * @return java.lang.reflect.Method
     * @author Jerry.X.He
     * @date 5/21/2017 4:36 PM
     * @since 1.0
     */
    public static Method lookForSetterMethod(Class clazz, String attr) {
        Tools.assert0(clazz != null, "'clazz' can't be null !");
        Tools.assert0(attr != null, "'attr' can't be null !");

        Method[] srcMethods = clazz.getDeclaredMethods();
        for (Method method : srcMethods) {
            int mod = method.getModifiers();
            if ((!Modifier.isStatic(mod))
                    && JSONParseUtils.startsWith(method.getName(), Constants.BEAN_SETTER_PREFIXES)) {
                if (Tools.equalsIgnoreCase(attr, JSONParseUtils.trimIfStartsWith(method.getName(), Constants.BEAN_SETTER_PREFIXES))) {
                    return method;
                }
            }
        }

        return null;
    }


    /**
     * 获取转换方法的方法名称
     *
     * @param srcClazzName srcClazzName
     * @param dstClazzName dstClazzName
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/21/2017 4:52 PM
     * @since 1.0
     */
    private static String getTransferMethodName(String srcClazzName, String dstClazzName) {
        return Tools.lowerCaseFirstChar(srcClazzName) + "2" + dstClazzName;
    }

}
